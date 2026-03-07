package io.github.nehaev.smarker

import Ast.TemplateBody
import Resolver.Context
import SmarkerScalaModel.SmarkerTypeOf
import SmarkerScalaModel.ToModel

final class CompiledTemplate[T] private[smarker] (
        val name: String,
        val body: TemplateBody,
        private[smarker] val toModel: ToModel[T],
)

final class TemplateSet private[smarker] (
        private val bodies: Map[String, TemplateBody]
) {

    def render[T](value: T)(using tm: ToModel[T], tpe: SmarkerTypeOf[T]): Either[SmarkerResolutionError, String] = {
        tpe.smarkerType match {
            case cls: SmarkerType.Class =>
                render(cls.name, value)
            case t =>
                render("non-class-fail", value)
        }
    }

    def render[T](template: String, value: T)(using tm: ToModel[T], tpe: SmarkerTypeOf[T]): Either[SmarkerResolutionError, String] = {
        val ctx = Context(Map(), bodies)
        tpe.smarkerType match {
            case cls: SmarkerType.Class =>
                bodies
                    .get(template)
                    .toRight(TemplateReferenceMissingError(template, cls, value, ctx, None))
                    .flatMap(body => Resolver.render(body, tm.apply(value), ctx))
            case t =>
                Left(TypeResolutionError("render requires a case class type", "class", t, value, ctx))
        }
    }

}

def template[T](src: String)(using tm: ToModel[T], tpe: SmarkerTypeOf[T]): Either[SmarkerParseError, CompiledTemplate[T]] = {
    // SmarkerTypeOf[T] for a case class (derived via Mirror.ProductOf) always yields SmarkerType.Class
    TemplateParser.parse(src).map { body => new CompiledTemplate[T](tpe.smarkerType.asInstanceOf[SmarkerType.Class].name, body, tm) }
}

def template[T](name: String, src: String)(using tm: ToModel[T]): Either[SmarkerParseError, CompiledTemplate[T]] = {
    TemplateParser.parse(src).map { body => new CompiledTemplate[T](name, body, tm) }
}

def templates(ts: Either[SmarkerParseError, CompiledTemplate[?]]*): Either[SmarkerError, TemplateSet] = {
    import cats.syntax.traverse._

    for {
        compiled <- ts.sequence
        byName = compiled.groupBy(_.name)
        // check for duplicate names
        _ <- byName.find(_._2.size > 1).map(c => Left(SmarkerDuplicateTemplateNameError(c._1))).getOrElse(Right(()))
    } yield new TemplateSet(compiled.map(t => t.name -> t.body).toMap)
}
