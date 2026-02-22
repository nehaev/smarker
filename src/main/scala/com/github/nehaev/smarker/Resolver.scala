package com.github.nehaev.smarker

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

    private val EmptyStringExpr = StringLiteral("")

    def render(template: TemplateBody, root: Model, ctx: Context): Either[SmarkerResolutionError, String] = {
        val sb = new StringBuilder()
        for {
            resolvedCtx <- resolveScope(root, ctx)
            _ <- renderBody(template.elements, resolvedCtx, sb)
        } yield sb.toString()
    }

    def resolveScope(root: Model, ctx: Context, alias: Option[String] = None): Either[SmarkerResolutionError, Context] = {
        val newScope = root match {
            case map: MapModel =>
                Right(map.keys.map(k => k -> map.get(k).get).toMap)
            case dyn: DynModel =>
                Right(dyn.keys.map(k => k -> dyn.get(k).get).toMap)
            case cls: ClassModel =>
                Right(root.getType match {
                    case SmarkerType.Class(_, fields) =>
                        fields.keys.map(k => k -> cls.get(k).get).toMap
                    case _ => ???
                })
            case unk =>
                Left(TypeResolutionError("Unable to init root scope", "map, class, or dyn", unk.getType, root, ctx))
        }
        for {
            fields <- newScope
            finalScope = alias.map(a => Map(a -> modelMapToDynModel(fields))).getOrElse(fields)
        } yield ctx.addScope(finalScope)
    }

    def renderBody(elements: List[TemplateElement], ctx: Context, sb: StringBuilder): Either[SmarkerResolutionError, Unit] = {
        elements.foldLeft(Right(()).withLeft[SmarkerResolutionError]) { (acc, e) => acc.flatMap(_ => renderTemplateElement(e, ctx, sb)) }
    }

    def renderTemplateElement(elem: TemplateElement, ctx: Context, sb: StringBuilder): Either[SmarkerResolutionError, Unit] = elem match {
        case Comment(text) =>
            Right(())
        case RawText(elements) =>
            elements.foldLeft(Right(()).withLeft[SmarkerResolutionError]) { (acc, e) => acc.flatMap(_ => renderRawTextElement(e, ctx, sb)) }
        case Interpolation(expr) =>
            renderExpr(expr, ctx, sb)
        case dirCall: DirectiveCall =>
            renderDirectiveCall(dirCall, ctx, sb)
    }

    def renderDirectiveCall(dirCall: DirectiveCall, ctx: Context, sb: StringBuilder): Either[SmarkerResolutionError, Unit] =
        dirCall.name.value match {
            case "ifDefined" => renderIfDefinedDirectiveCall(dirCall, ctx, sb)
            case "list"      => renderListDirectiveCall(dirCall, ctx, sb)
            case "block"     => renderBlockDirectiveCall(dirCall, ctx, sb)
        }

    def renderIfDefinedDirectiveCall(dirCall: DirectiveCall, ctx: Context, sb: StringBuilder): Either[SmarkerResolutionError, Unit] = {
        val name = dirCall.name

        def renderValueInBody(m: Model, body: List[TemplateElement]): Either[SmarkerResolutionError, Unit] = {
            for {
                alias <- resolveStringParam(dirCall.args, "as", "_", ctx)
                newCtx <- m match {
                    case _: MapModel | _: ClassModel | _: DynModel => resolveScope(m, ctx, Some(alias))
                    case _                                         => Right(ctx.addScope(Map(alias -> m)))
                }
                _ <- renderBody(body, newCtx, sb)
            } yield ()
        }

        def renderValueUsingTemplateRef(m: Model): Either[SmarkerResolutionError, Unit] = {
            for {
                targetType <- m match {
                    case c: ClassModel => Right(c.getType)
                    case unk =>
                        Left(
                            TypeResolutionError(
                                "Unexpected value type for body-less",
                                "class",
                                unk.getType,
                                unk.getUnderlying,
                                ctx,
                                Some(name.span),
                            )
                        )
                }
                templateName = targetType.asInstanceOf[SmarkerType.Class].name
                templateBody <- ctx.templateRefs
                    .get(templateName)
                    .toRight(TemplateReferenceMissingError(targetType, m.getUnderlying, ctx, Some(name.span)))
                newCtx <- resolveScope(m, ctx.clearScope)
                _ <- renderBody(templateBody.elements, newCtx, sb)
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
                case (Some(m), Some(body)) => renderValueInBody(m, body)
                case (Some(m), None)       => renderValueUsingTemplateRef(m)
                case (None, _)             => altExprO.fold(Right(()))(renderExpr(_, ctx, sb))
            }
        } yield ()
    }

    def renderListDirectiveCall(dirCall: DirectiveCall, ctx: Context, sb: StringBuilder): Either[SmarkerResolutionError, Unit] = {
        val name = dirCall.name

        def renderItemInBody(item: Model, body: List[TemplateElement]): Either[SmarkerResolutionError, Unit] = {
            for {
                alias <- resolveStringParam(dirCall.args, "as", "_", ctx)
                newCtx <- item match {
                    case _: MapModel | _: ClassModel | _: DynModel => resolveScope(item, ctx, Some(alias))
                    case _                                         => Right(ctx.addScope(Map(alias -> item)))
                }
                _ <- renderBody(body, newCtx, sb)
            } yield ()
        }

        def renderItemUsingTemplateRef(item: Model): Either[SmarkerResolutionError, Unit] = {
            for {
                targetType <- item match {
                    case c: ClassModel => Right(c.getType)
                    case unk =>
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
                templateName = targetType.asInstanceOf[SmarkerType.Class].name
                templateBody <- ctx.templateRefs
                    .get(templateName)
                    .toRight(TemplateReferenceMissingError(targetType, item.getUnderlying, ctx, Some(name.span)))
                newCtx <- resolveScope(item, ctx.clearScope)
                _ <- renderBody(templateBody.elements, newCtx, sb)
            } yield ()
        }

        for {
            itemsExpr <- dirCall.args.get("items").toRight(RequiredParamMissingError(name.value, "items", ctx, Some(name.span)))
            itemsModel <- resolveExpr(itemsExpr, ctx)
            listModel <- itemsModel match {
                case lm: ListModel => Right(lm)
                case other         => Left(TypeResolutionError("items must be a list", "list", other.getType, other, ctx, Some(name.span)))
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
                            bodyO match {
                                case Some(body) => renderItemInBody(item, body)
                                case None       => renderItemUsingTemplateRef(item)
                            }
                        }
                    }
                    result.map(_ => write(end, ctx, sb))
                }
            }
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
            _ <- renderBody(body, innerCtx, sb)
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
            str <- modelToString(model, ctx)
        } yield write(str, ctx, sb)
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

    def resolveStringParam(args: Map[String, Expr], key: String, default: String, ctx: Context): Either[SmarkerResolutionError, String] =
        resolveParam(args, key, default, ctx)(modelToString(_, ctx))

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
                Left(TypeResolutionError("Unable to select field", "map, class, or dyn", unk.getType, unk, ctx, Some(select.field.span)))
        }
    }

    private def modelFromScope(name: String, ctx: Context): Either[SmarkerResolutionError, Model] = {
        ctx.scope.get(name).toRight(ScopePathMissingError(name, ctx.scope, ctx))
    }

    private def modelToString(model: Model, ctx: Context): Either[SmarkerResolutionError, String] = model match {
        case pm: PrimitiveModel => Right(pm.getAsString)
        case _                  => Left(TypeResolutionError(s"Unable to render", "primitive", model.getType, model, ctx))
    }

    private def write(text: String, ctx: Context, sb: StringBuilder): Unit = {
        val prefix = ctx.indentChar * (ctx.indentSize * ctx.indentLevel)
        val atLineStart = sb.isEmpty || sb.last == '\n' // true when the next char starts a new line
        val lines = text.split("\n", -1) // -1 preserves trailing empty segments
        val result = lines.zipWithIndex
            .map { (line, i) =>
                // First segment: only indent if we're at a line start (may be a continuation).
                // Later segments: always indent — they follow an embedded newline.
                // Never indent empty lines (would leave trailing whitespace).
                val needsPrefix = line.nonEmpty && (if (i == 0) atLineStart else true)
                if (needsPrefix) prefix + line else line
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
