package io.github.nehaev.smarker

import scala.annotation.tailrec
import Ast.*
import SmarkerModel.*

object Resolver {

    case class Context(
            scope: Map[String, Model],
            templateRefs: Map[String, TemplateBody],
            indentChar: String = " ",
            indentSize: Int = 4,
            indentLevel: Int = 0,
    ) {
        def addScope(toAdd: Map[String, Model]): Context = copy(scope = scope ++ toAdd)
        def clearScope: Context = copy(scope = Map())
    }

    def render(template: TemplateBody, root: Model, ctx: Context): Either[SmarkerResolutionError, String] = {
        val sb = new StringBuilder()
        for {
            resolvedCtx <- Impl.resolveScope(root, ctx)
            _ <- Impl.renderBody(template.elements, resolvedCtx, sb)
        } yield sb.toString()
    }

    private[smarker] object Impl {

        def resolveScope(root: Model, ctx: Context): Either[SmarkerResolutionError, Context] = {
            val newScope = root match {
                case map: MapModel =>
                    Right(map.keys.map(k => k -> map.get(k).get).toMap)
                case dyn: DynModel =>
                    Right(dyn.keys.map(k => k -> dyn.get(k).get).toMap)
                case cls: ClassModel =>
                    Right(root.getType match {
                        case SmarkerType.Class(_, fieldsSupplier) =>
                            fieldsSupplier().keys.map(k => k -> cls.get(k).get).toMap
                        case _ => ???
                    })
                case unk =>
                    Left(TypeResolutionError("Unable to init root scope", "map, class, or dyn", unk.getType, root, ctx))
            }
            for {
                fields <- newScope
            } yield ctx.addScope(fields)
        }

        def renderBody(elements: List[TemplateElement], ctx: Context, sb: StringBuilder): Either[SmarkerResolutionError, Unit] = {
            val processed = applyWhitespaceControl(elements, stripLeadingNewline = false)
            processed.foldLeft(Right(()).withLeft[SmarkerResolutionError]) { (acc, e) =>
                acc.flatMap(_ => renderTemplateElement(e, ctx, sb))
            }
        }

        private def renderDirectiveBody(
                elements: List[TemplateElement],
                ctx: Context,
                sb: StringBuilder,
        ): Either[SmarkerResolutionError, Unit] = {
            val processed = applyWhitespaceControl(elements, stripLeadingNewline = true)
            processed.foldLeft(Right(()).withLeft[SmarkerResolutionError]) { (acc, e) =>
                acc.flatMap(_ => renderTemplateElement(e, ctx, sb))
            }
        }

        private def applyWhitespaceControl(elements: List[TemplateElement], stripLeadingNewline: Boolean): List[TemplateElement] = {
            def stripFirstNewline(rt: RawText): RawText = rt.elements match {
                case RawNewLine :: rest =>
                    val trimmed = rest.dropWhile {
                        case RawString(s) => s.isBlank
                        case _            => false
                    }
                    RawText(trimmed)
                case _ => rt
            }

            // Strip the leading newline from the body start when the opening directive tag
            // occupied its own line (body begins immediately with a newline).
            val afterLeadingStrip = if (stripLeadingNewline) {
                elements match {
                    case (rt: RawText) :: tail if rt.elements.headOption.contains(RawNewLine) =>
                        stripFirstNewline(rt) :: tail
                    case _ => elements
                }
            } else elements

            // Strip the first newline following each DirectiveCall or Comment, since those
            // elements occupy directive-only lines whose trailing newline must be suppressed.
            // Only the first of multiple consecutive newlines is stripped (the others remain).
            @tailrec
            def loop(rem: List[TemplateElement], acc: List[TemplateElement], stripNext: Boolean): List[TemplateElement] = rem match {
                case Nil                                      => acc.reverse
                case (rt: RawText) :: tail if stripNext       => loop(tail, stripFirstNewline(rt) :: acc, false)
                case (dc @ DirectiveCall(_, _, None)) :: tail => loop(tail, dc :: acc, false)
                case (dc: DirectiveCall) :: tail              => loop(tail, dc :: acc, true)
                case (c: Comment) :: tail                     => loop(tail, c :: acc, true)
                case head :: tail                             => loop(tail, head :: acc, false)
            }

            loop(afterLeadingStrip, Nil, false)
        }

        def renderTemplateElement(elem: TemplateElement, ctx: Context, sb: StringBuilder): Either[SmarkerResolutionError, Unit] =
            elem match {
                case Comment(text) =>
                    Right(())
                case RawText(elements) =>
                    elements.foldLeft(Right(()).withLeft[SmarkerResolutionError]) { (acc, e) =>
                        acc.flatMap(_ => renderRawTextElement(e, ctx, sb))
                    }
                case Interpolation(expr) =>
                    renderExpr(expr, ctx, sb)
                case dirCall: DirectiveCall =>
                    renderDirectiveCall(dirCall, ctx, sb)
            }

        def renderDirectiveCall(dirCall: DirectiveCall, ctx: Context, sb: StringBuilder): Either[SmarkerResolutionError, Unit] =
            dirCall.name.value match {
                case "if"        => renderIfDirectiveCall(dirCall, ctx, sb)
                case "ifDefined" => renderIfDefinedDirectiveCall(dirCall, ctx, sb)
                case "list"      => renderListDirectiveCall(dirCall, ctx, sb)
                case "block"     => renderBlockDirectiveCall(dirCall, ctx, sb)
            }

        def renderIfDefinedDirectiveCall(dirCall: DirectiveCall, ctx: Context, sb: StringBuilder): Either[SmarkerResolutionError, Unit] = {
            val name = dirCall.name

            // TODO: renderValueInBody and renderItemInBody (in renderListDirectiveCall) are identical — extract as a shared private helper
            def renderValueInBody(m: Model, body: List[TemplateElement]): Either[SmarkerResolutionError, Unit] = {
                for {
                    alias <- resolveStringParam(dirCall.args, "as", "_", ctx)
                    newCtx = ctx.addScope(Map(alias -> m))
                    _ <- renderDirectiveBody(body, newCtx, sb)
                } yield ()
            }

            for {
                valueExpr <- dirCall.args.get("value").toRight(RequiredParamMissingError(name.value, "value", ctx, Some(name.span)))
                altExprO = dirCall.args.get("alt")
                bodyO = dirCall.body
                valueModelO = resolveExpr(valueExpr, ctx).toOption // TODO: explicit err check?
                resolvedValueModelO = valueModelO.flatMap {
                    case m: OptModel => Option.unless(m.isEmpty)(m.get)
                    case other       => Some(other)
                }
                _ <- (resolvedValueModelO, bodyO) match {
                    case (Some(m), Some(body))        => renderValueInBody(m, body)
                    case (Some(cm: ClassModel), None) => renderClassModelUsingTemplateRef(cm, ctx, sb)
                    case (Some(unk), _) =>
                        Left(
                            TypeResolutionError(
                                "Unexpected value type for body-less ifDefined",
                                "class",
                                unk.getType,
                                unk.getUnderlying,
                                ctx,
                                Some(name.span),
                            )
                        )
                    case (None, _) => altExprO.fold(Right(()))(renderExpr(_, ctx, sb))
                }
            } yield ()
        }

        def renderListDirectiveCall(dirCall: DirectiveCall, ctx: Context, sb: StringBuilder): Either[SmarkerResolutionError, Unit] = {
            val name = dirCall.name

            def renderItemInBody(item: Model, body: List[TemplateElement]): Either[SmarkerResolutionError, Unit] = {
                for {
                    alias <- resolveStringParam(dirCall.args, "as", "_", ctx)
                    newCtx = ctx.addScope(Map(alias -> item))
                    _ <- renderDirectiveBody(body, newCtx, sb)
                } yield ()
            }

            for {
                itemsExpr <- dirCall.args.get("items").toRight(RequiredParamMissingError(name.value, "items", ctx, Some(name.span)))
                itemsModel <- resolveExpr(itemsExpr, ctx)
                listModel <- itemsModel match {
                    case lm: ListModel => Right(lm)
                    case other => Left(TypeResolutionError("items must be a list", "list", other.getType, other, ctx, Some(name.span)))
                }
                sep <- resolveStringParam(dirCall.args, "sep", ", ", ctx)
                start <- resolveStringParam(dirCall.args, "start", "", ctx)
                end <- resolveStringParam(dirCall.args, "end", "", ctx)
                bodyO = dirCall.body
                items = listModel.iterable.toList
                _ <- {
                    // start/end are omitted entirely for an empty list
                    if (items.isEmpty) Right(())
                    else {
                        write(start, ctx, sb)
                        val result = items.zipWithIndex.foldLeft(Right(()).withLeft[SmarkerResolutionError]) { (acc, entry) =>
                            val (item, i) = entry
                            acc.flatMap { _ =>
                                // sep is between elements, not after the last one
                                if (i > 0) write(sep, ctx, sb)
                                (item, bodyO) match {
                                    case (_, Some(body))        => renderItemInBody(item, body)
                                    case (cm: ClassModel, None) => renderClassModelUsingTemplateRef(cm, ctx, sb)
                                    case (unk, None) =>
                                        Left(
                                            TypeResolutionError(
                                                "Unexpected value type for body-less list",
                                                "class",
                                                unk.getType,
                                                unk.getUnderlying,
                                                ctx,
                                                Some(name.span),
                                            )
                                        )
                                }
                            }
                        }
                        result.map(_ => write(end, ctx, sb))
                    }
                }
            } yield ()
        }

        def renderIfDirectiveCall(dirCall: DirectiveCall, ctx: Context, sb: StringBuilder): Either[SmarkerResolutionError, Unit] = {
            val name = dirCall.name
            for {
                _ <- dirCall.args.get("cond").toRight(RequiredParamMissingError(name.value, "cond", ctx, Some(name.span)))
                cond <- resolveBoolParam(dirCall.args, "cond", false, ctx)
                elements <- dirCall.body.toRight(RequiredParamMissingError(name.value, "body", ctx, Some(name.span)))
                _ <- if (cond) renderDirectiveBody(elements, ctx, sb) else Right(())
            } yield ()
        }

        def renderBlockDirectiveCall(dirCall: DirectiveCall, ctx: Context, sb: StringBuilder): Either[SmarkerResolutionError, Unit] = {
            val name = dirCall.name
            for {
                body <- dirCall.body.toRight(RequiredParamMissingError(name.value, "body", ctx, Some(name.span)))
                identChar <- resolveStringParam(dirCall.args, "identChar", ctx.indentChar, ctx)
                identSize <- resolveIntParam(dirCall.args, "identSize", ctx.indentSize, ctx, name.span)
                start <- resolveStringParam(dirCall.args, "start", "", ctx)
                end <- resolveStringParam(dirCall.args, "end", "", ctx)
                innerCtx = ctx.copy(indentChar = identChar, indentSize = identSize, indentLevel = ctx.indentLevel + 1)
                _ = write(start, ctx, sb)
                _ <- renderDirectiveBody(body, innerCtx, sb)
                _ = write(end, ctx, sb)
            } yield ()
        }

        def renderRawTextElement(elem: RawTextElement, ctx: Context, sb: StringBuilder): Either[SmarkerResolutionError, Unit] = elem match {
            case RawString(value) => Right(write(value, ctx, sb))
            case RawNewLine       => Right(write("\n", ctx, sb))
        }

        def renderExpr(expr: Expr, ctx: Context, sb: StringBuilder): Either[SmarkerResolutionError, Unit] = {
            for {
                model <- resolveExpr(expr, ctx)
                str <- renderModel(model, ctx, sb)
            } yield ()
        }

        def renderModel(model: Model, ctx: Context, sb: StringBuilder): Either[SmarkerResolutionError, Unit] = model match {
            case pm: PrimitiveModel => Right(write(pm.getAsString, ctx, sb))
            case cm: ClassModel     => renderClassModelUsingTemplateRef(cm, ctx, sb)
            case _                  => Left(TypeResolutionError(s"Unable to render", "primitive or class", model.getType, model, ctx))
        }

        def renderClassModelUsingTemplateRef(cm: ClassModel, ctx: Context, sb: StringBuilder): Either[SmarkerResolutionError, Unit] = {
            for {
                targetType <- Right(cm.getType)
                templateName = targetType.asInstanceOf[SmarkerType.Class].name
                templateBody <- ctx.templateRefs
                    .get(templateName)
                    .toRight(TemplateReferenceMissingError(templateName, targetType, cm.getUnderlying, ctx, None)) // TODO: span
                newCtx <- resolveScope(cm, ctx.clearScope)
                _ <- renderDirectiveBody(templateBody.elements, newCtx, sb)
            } yield ()
        }

        def resolveParam[T](
                args: Map[String, Expr],
                key: String,
                default: T,
                ctx: Context,
        )(extract: Model => Either[SmarkerResolutionError, T]): Either[SmarkerResolutionError, T] =
            args.get(key) match {
                case None       => Right(default)
                case Some(expr) => resolveExpr(expr, ctx).flatMap(extract)
            }

        def resolveStringParam(
                args: Map[String, Expr],
                key: String,
                default: String,
                ctx: Context,
        ): Either[SmarkerResolutionError, String] =
            resolveParam(args, key, default, ctx) { m =>
                if m.getType == SmarkerType.String then Right(m.getUnderlying[String])
                else Left(TypeResolutionError(s"Wrong type for param '$key'", "string", m.getType, m, ctx, None)) // TODO: span
            }

        def resolveIntParam(
                args: Map[String, Expr],
                key: String,
                default: Int,
                ctx: Context,
                span: SpanLike,
        ): Either[SmarkerResolutionError, Int] =
            resolveParam(args, key, default, ctx) { m =>
                if m.getType == SmarkerType.Int then Right(m.getUnderlying[Int])
                else Left(TypeResolutionError(s"Wrong type for param '$key'", "int", m.getType, m, ctx, Some(span)))
            }

        def resolveBoolParam(
                args: Map[String, Expr],
                key: String,
                default: Boolean,
                ctx: Context,
        ): Either[SmarkerResolutionError, Boolean] =
            resolveParam(args, key, default, ctx) { m =>
                if m.getType == SmarkerType.Bool then Right(m.getUnderlying[Boolean])
                else Left(TypeResolutionError(s"Wrong type for param '$key'", "bool", m.getType, m, ctx, None))
            }

        def resolveExpr(expr: Expr, ctx: Context): Either[SmarkerResolutionError, Model] = {
            import SmarkerScalaModel.ToModel
            expr match {
                case BoolLiteral(value)   => Right(ToModel[Boolean].apply(value))
                case IntLiteral(value)    => Right(ToModel[Int].apply(value))
                case StringLiteral(value) => Right(ToModel[String].apply(value))
                case ident: Ident         => resolveIdent(ident, ctx)
                case select: Select       => resolveSelect(select, ctx)
            }
        }

        def resolveIdent(ident: Ident, ctx: Context): Either[SmarkerResolutionError, Model] = {
            modelFromScope(ident.name.value, ctx)
        }

        def resolveSelect(select: Select, ctx: Context): Either[SmarkerResolutionError, Model] = {
            val rec = select.obj match {
                case ident: Ident =>
                    resolveIdent(ident, ctx)
                case sel: Select =>
                    resolveSelect(sel, ctx)
            }
            rec.flatMap {
                case model: MapModel =>
                    model.get(select.field.value).toRight(ScopePathMissingError(select.field.value, model, ctx, Some(select.field.span)))
                case model: DynModel =>
                    model.get(select.field.value).toRight(ScopePathMissingError(select.field.value, model, ctx, Some(select.field.span)))
                case model: ClassModel =>
                    model.get(select.field.value).toRight(ScopePathMissingError(select.field.value, model, ctx, Some(select.field.span)))
                case unk =>
                    Left(
                        TypeResolutionError("Unable to select field", "map, class, or dyn", unk.getType, unk, ctx, Some(select.field.span))
                    )
            }
        }

        private def modelFromScope(name: String, ctx: Context): Either[SmarkerResolutionError, Model] = {
            ctx.scope.get(name).toRight(ScopePathMissingError(name, ctx.scope, ctx))
        }

        private def write(text: String, ctx: Context, sb: StringBuilder): Unit = {
            val prefix = ctx.indentChar * (ctx.indentSize * ctx.indentLevel)
            val atLineStart = sb.isEmpty || sb.last == '\n' // true when the next char starts a new line
            val lines = text.split("\n", -1) // -1 preserves trailing empty segments
            val result = lines.zipWithIndex
                .map { (line, i) =>
                    // i > 0 always follows an embedded newline; i == 0 only if already at a line start.
                    val isAtLineStart = i > 0 || (i == 0 && atLineStart)
                    val stripped = if (isAtLineStart) line.dropWhile(c => c == ' ' || c == '\t') else line
                    // First segment: only indent if we're at a line start (may be a continuation).
                    // Later segments: always indent — they follow an embedded newline.
                    // Never indent empty lines (would leave trailing whitespace).
                    val needsPrefix = stripped.nonEmpty && (if (i == 0) atLineStart else true)
                    if (needsPrefix) prefix + stripped else stripped
                }
                .mkString("\n")
            sb.append(result)
        }

        private def modelMapToDynModel(map: Map[String, Model]): DynModel = new DynModel {
            def getType: SmarkerType = SmarkerType.Dyn
            def keys: Iterable[String] = map.keys
            def get(field: String): Option[Model] = map.get(field)
            def getUnderlyingObject: Any = map
        }
    }

}
