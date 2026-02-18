package com.github.nehaev.smarker

import Ast.*
import SmarkerModel.*

object Resolver {

    case class Context(scope: Map[String, Model])

    case class ResolutionError(message: String)

    private val EmptyStringExpr = StringLiteral("")

    def render(template: TemplateBody, root: Model, ctx: Context): Either[ResolutionError, String] = {
        val sb = new StringBuilder()
        for {
            resolvedCtx <- resolveScope(root, ctx)
            _ <- renderBody(template.elements, resolvedCtx, sb)
        } yield sb.toString()
    }

    def resolveScope(root: Model, ctx: Context, alias: Option[String] = None): Either[ResolutionError, Context] = {
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
                Left(ResolutionError(s"Root model must be a map or class to add it to a scope, but was ${unk.getType}"))
        }
        for {
            fields <- newScope
            finalScope = alias.map(a => Map(a -> modelMapToClassModel(fields))).getOrElse(fields)
        } yield Context(ctx.scope ++ finalScope)
    }

    def renderBody(elements: List[TemplateElement], ctx: Context, sb: StringBuilder): Either[ResolutionError, Unit] = {
        elements.foldLeft(Right(()).withLeft[ResolutionError]) { (acc, e) => acc.flatMap(_ => renderTemplateElement(e, ctx, sb)) }
    }

    def renderTemplateElement(elem: TemplateElement, ctx: Context, sb: StringBuilder): Either[ResolutionError, Unit] = elem match {
        case Comment(text) =>
            Right(())
        case RawText(elements) =>
            elements.foldLeft(Right(()).withLeft[ResolutionError]) { (acc, e) => acc.flatMap(_ => renderRawTextElement(e, sb)) }
        case Interpolation(expr) =>
            renderExpr(expr, ctx, sb)
        case dirCall: DirectiveCall =>
            renderDirectiveCall(dirCall, ctx, sb)
    }

    def renderDirectiveCall(dirCall: DirectiveCall, ctx: Context, sb: StringBuilder): Either[ResolutionError, Unit] = dirCall.name match {
        case "ifDefined" => renderIfDefinedDirectiveCall(dirCall, ctx, sb)
    }

    def renderIfDefinedDirectiveCall(dirCall: DirectiveCall, ctx: Context, sb: StringBuilder): Either[ResolutionError, Unit] = {
        val valueExprE = dirCall.args.get("value").toRight(ResolutionError("No `value` arg provided for #ifDefined directive"))
        val altExprO = dirCall.args.get("alt")
        val aliasExprO = dirCall.args.get("as")

        val bodyE = dirCall.body.toRight(ResolutionError("Body is required for #ifDefined"))

        def renderValueInBody(m: Model): Either[ResolutionError, Unit] = {
            for {
                body <- bodyE
                alias = aliasExprO.flatMap(a => resolveExpr(a, ctx).flatMap(modelToString).toOption).orElse(Some("_"))
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

    def renderRawTextElement(elem: RawTextElement, sb: StringBuilder): Either[ResolutionError, Unit] = elem match {
        case RawString(value) => Right(sb.append(value))
        case RawNewLine       => Right(sb.append("\n"))
    }

    def renderExpr(expr: Expr, ctx: Context, sb: StringBuilder): Either[ResolutionError, Unit] = {
        for {
            model <- resolveExpr(expr, ctx)
            str <- modelToString(model)
        } yield sb.append(str)
    }

    def resolveExpr(expr: Expr, ctx: Context): Either[ResolutionError, Model] = {
        import SmarkerScalaModel.ToModel
        expr match {
            case BoolLiteral(value)   => Right(ToModel[Boolean].apply(value))
            case IntLiteral(value)    => Right(ToModel[Int].apply(value))
            case StringLiteral(value) => Right(ToModel[String].apply(value))
            case ident: Ident         => resolveIdent(ident, ctx)
            case select: Select       => resolveSelect(select, ctx)
        }
    }

    def resolveIdent(ident: Ident, ctx: Context): Either[ResolutionError, Model] = {
        modelFromScope(ident.name, ctx)
    }

    def resolveSelect(select: Select, ctx: Context): Either[ResolutionError, Model] = {
        val rec = select.obj match {
            case ident: Ident =>
                resolveIdent(ident, ctx)
            case sel: Select =>
                resolveSelect(sel, ctx)
        }
        rec.flatMap {
            case model: MapModel =>
                model.get(select.field).toRight(ResolutionError(s"Undefined field: ${select.field} on ${select.obj}"))
            case model: ClassModel =>
                model.get(select.field).toRight(ResolutionError(s"Undefined field: ${select.field} on ${select.obj}"))
            case unk =>
                Left(ResolutionError(s"Cannot select field on: ${unk.getType}"))
        }
    }

    def modelFromScope(name: String, ctx: Context): Either[ResolutionError, Model] = {
        ctx.scope.get(name).toRight(ResolutionError(s"Undefined variable: $name"))
    }

    def modelToString(model: Model): Either[ResolutionError, String] = model match {
        case pm: PrimitiveModel => Right(pm.getAsString)
        case _                  => Left(ResolutionError(s"Cannot render non-primitive model: ${model.getType}"))
    }

    def isModelEmpty(model: Model): Either[ResolutionError, Boolean] = model match {
        case m: OptModel  => Right(m.isEmpty)
        case m: ListModel => Right(m.isEmpty)
        case m: MapModel  => Right(!m.keys.iterator.hasNext)
        case NothingModel => Right(true)
        case _            => Left(ResolutionError(s"Cannot check emptiness of ${model.getType}"))
    }

    def modelMapToClassModel(map: Map[String, Model]): ClassModel = new ClassModel {
        def getType: SmarkerType = SmarkerType.Class(map.map((k, v) => k -> v.getType))
        def get(field: String): Option[Model] = map.get(field)
    }

}
