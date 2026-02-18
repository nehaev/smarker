package com.github.nehaev.smarker

object Ast {

    // DEBUG AND OBSERVABILITY INFO
    case class Span(startOffset: Int, endOffset: Int)
    case class WithSpan[+T](value: T, span: Span) derives CanEqual

    // EXPRESSIONS
    sealed trait Expr derives CanEqual

    sealed trait Literal extends Expr
    case class StringLiteral(value: String) extends Literal
    case class BoolLiteral(value: Boolean) extends Literal
    case class IntLiteral(value: Int) extends Literal

    case class Ident(name: String) extends Expr
    case class Select(obj: Select | Ident, field: String) extends Expr

    // TEMPLATE STRUCTURE
    sealed trait TemplateElement derives CanEqual

    sealed trait RawTextElement derives CanEqual
    case class RawString(value: String) extends RawTextElement
    case object RawNewLine extends RawTextElement
    case class RawText(elements: List[RawTextElement]) extends TemplateElement

    case class Comment(text: String) extends TemplateElement
    case class Interpolation(expr: Expr) extends TemplateElement

    case class DirectiveCall(name: String, args: Map[String, Expr], body: Option[List[TemplateElement]]) extends TemplateElement

    case class TemplateBody(elements: List[TemplateElement]) derives CanEqual

}
