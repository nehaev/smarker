package io.github.nehaev.smarker

object Ast {

    // DEBUG AND OBSERVABILITY INFO
    sealed trait SpanLike derives CanEqual
    object SpanLike {
        given Conversion[SpanLike, Span] = _ match {
            case s: Span => s
            case AnySpan => ??? // never reached in the main code
        }
    }
    case class Span(startOffset: Int, endOffset: Int) extends SpanLike {
        override def equals(that: Any): Boolean = that match {
            case Span(s, e)      => startOffset == s && endOffset == e
            case _: AnySpan.type => true
            case _               => false
        }
    }
    private[smarker] case object AnySpan extends SpanLike {
        override def equals(that: Any): Boolean = that match {
            case _: Span => true
            case _       => this eq that.asInstanceOf[AnyRef]
        }
        override def hashCode: Int = 0
    }

    case class WithSpan[+T](value: T, span: SpanLike) derives CanEqual

    // EXPRESSIONS
    sealed trait Expr derives CanEqual

    sealed trait Literal extends Expr
    case class StringLiteral(value: String) extends Literal
    case class BoolLiteral(value: Boolean) extends Literal
    case class IntLiteral(value: Int) extends Literal

    case class Ident(name: WithSpan[String]) extends Expr
    case class Select(obj: Select | Ident, field: WithSpan[String]) extends Expr

    // TEMPLATE STRUCTURE
    sealed trait TemplateElement derives CanEqual

    sealed trait RawTextElement derives CanEqual
    case class RawString(value: String) extends RawTextElement
    case object RawNewLine extends RawTextElement
    case class RawText(elements: List[RawTextElement]) extends TemplateElement

    case class Comment(text: String) extends TemplateElement
    case class Interpolation(expr: Expr) extends TemplateElement

    case class DirectiveCall(name: WithSpan[String], args: Map[String, Expr], body: Option[List[TemplateElement]]) extends TemplateElement

    case class TemplateBody(elements: List[TemplateElement]) derives CanEqual

}
