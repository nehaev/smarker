package com.github.nehaev.smarker

import Ast.TemplateBody
import Resolver.Context
import SmarkerScalaModel.SmarkerTypeOf
import SmarkerScalaModel.ToModel

final class CompiledTemplate[T] private[smarker] (
        val typeName: String,
        val body: TemplateBody,
        private[smarker] val toModel: ToModel[T],
)

final class TemplateSet private[smarker] (
        private val bodies: Map[String, TemplateBody]
) {

    def render[T](value: T)(using tm: ToModel[T], tpe: SmarkerTypeOf[T]): Either[SmarkerResolutionError, String] = {
        val ctx = Context(Map(), bodies)
        tpe.smarkerType match {
            case cls: SmarkerType.Class =>
                bodies
                    .get(cls.name)
                    .toRight(TemplateReferenceMissingError(cls, value, ctx, None))
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

def templates(ts: Either[SmarkerParseError, CompiledTemplate[?]]*): Either[SmarkerParseError, TemplateSet] = {
    ts.foldLeft(Right(Vector.empty[CompiledTemplate[?]]).withLeft[SmarkerParseError]) { (acc, t) =>
        for { prev <- acc; next <- t } yield prev :+ next
    }.map { compiled => new TemplateSet(compiled.map(t => t.typeName -> t.body).toMap) }
}
