package com.github.nehaev.smarker

import Ast.*
import SmarkerModel.*

object Resolver {

    case class Context(scope: Map[String, Model])

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
            case cls: ClassModel =>
                Right(root.getType match {
                    case SmarkerType.Class(fields) =>
                        fields.keys.map(k => k -> cls.get(k).get).toMap
                    case _ => ???
                })
            case unk =>
                Left(TypeResolutionError("Unable to init root scope", "map or class", unk.getType, root, ctx))
        }
        for {
            fields <- newScope
            finalScope = alias.map(a => Map(a -> modelMapToClassModel(fields))).getOrElse(fields)
        } yield Context(ctx.scope ++ finalScope)
    }

    def renderBody(elements: List[TemplateElement], ctx: Context, sb: StringBuilder): Either[SmarkerResolutionError, Unit] = {
        elements.foldLeft(Right(()).withLeft[SmarkerResolutionError]) { (acc, e) => acc.flatMap(_ => renderTemplateElement(e, ctx, sb)) }
    }

    def renderTemplateElement(elem: TemplateElement, ctx: Context, sb: StringBuilder): Either[SmarkerResolutionError, Unit] = elem match {
        case Comment(text) =>
            Right(())
        case RawText(elements) =>
            elements.foldLeft(Right(()).withLeft[SmarkerResolutionError]) { (acc, e) => acc.flatMap(_ => renderRawTextElement(e, sb)) }
        case Interpolation(expr) =>
            renderExpr(expr, ctx, sb)
        case dirCall: DirectiveCall =>
            renderDirectiveCall(dirCall, ctx, sb)
    }

    def renderDirectiveCall(dirCall: DirectiveCall, ctx: Context, sb: StringBuilder): Either[SmarkerResolutionError, Unit] =
        dirCall.name.value match {
            case "ifDefined" => renderIfDefinedDirectiveCall(dirCall, ctx, sb)
        }

    def renderIfDefinedDirectiveCall(dirCall: DirectiveCall, ctx: Context, sb: StringBuilder): Either[SmarkerResolutionError, Unit] = {
        val name = dirCall.name
        val valueExprE = dirCall.args
            .get("value")
            .toRight(RequiredParamMissingError(name.value, "value", ctx, Some(name.span)))
        val altExprO = dirCall.args.get("alt")
        val aliasExprO = dirCall.args.get("as")

        val bodyE = dirCall.body.toRight(RequiredParamMissingError(name.value, "body", ctx, Some(name.span)))

        def renderValueInBody(m: Model): Either[SmarkerResolutionError, Unit] = {
            for {
                body <- bodyE
                alias = aliasExprO
                    .flatMap { a =>
                        resolveExpr(a, ctx)
                            .flatMap(m => modelToString(m, ctx))
                            .toOption
                    }
                    .orElse(Some("_"))
                newCtx <- m match {
                    case _: MapModel | _: ClassModel => resolveScope(m, ctx, alias)
                    case _                           => Right(Context(ctx.scope ++ alias.map(_ -> m).toMap))
                }
                _ <- renderBody(body, newCtx, sb)
            } yield ()
        }

        for {
            valueExpr <- valueExprE
            valueModelO = resolveExpr(valueExpr, ctx).toOption // TODO: explicit err check?
            resolvedValueModelO = valueModelO.flatMap {
                case m: OptModel => Option.unless(m.isEmpty)(m.get)
                case other       => Some(other)
            }
            // render value if defined
            _ <- if (!resolvedValueModelO.isEmpty) renderValueInBody(resolvedValueModelO.get) else Right(())
            // otherwise render alt if defined
            _ <- if (resolvedValueModelO.isEmpty && !altExprO.isEmpty) renderExpr(altExprO.get, ctx, sb) else Right(())
        } yield ()
    }

    def renderRawTextElement(elem: RawTextElement, sb: StringBuilder): Either[SmarkerResolutionError, Unit] = elem match {
        case RawString(value) => Right(sb.append(value))
        case RawNewLine       => Right(sb.append("\n"))
    }

    def renderExpr(expr: Expr, ctx: Context, sb: StringBuilder): Either[SmarkerResolutionError, Unit] = {
        for {
            model <- resolveExpr(expr, ctx)
            str <- modelToString(model, ctx)
        } yield sb.append(str)
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
            case model: ClassModel =>
                model.get(select.field.value).toRight(ScopePathMissingError(select.field.value, model, ctx, Some(select.field.span)))
            case unk =>
                Left(TypeResolutionError("Unable to select field", "map or class", unk.getType, unk, ctx, Some(select.field.span)))
        }
    }

    private def modelFromScope(name: String, ctx: Context): Either[SmarkerResolutionError, Model] = {
        ctx.scope.get(name).toRight(ScopePathMissingError(name, ctx.scope, ctx))
    }

    private def modelToString(model: Model, ctx: Context): Either[SmarkerResolutionError, String] = model match {
        case pm: PrimitiveModel => Right(pm.getAsString)
        case _                  => Left(TypeResolutionError(s"Unable to render", "primitive", model.getType, model, ctx))
    }

    private def modelMapToClassModel(map: Map[String, Model]): ClassModel = new ClassModel {
        def getType: SmarkerType = SmarkerType.Class(map.map((k, v) => k -> v.getType))
        def get(field: String): Option[Model] = map.get(field)
    }

}
