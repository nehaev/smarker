package io.github.nehaev.smarker

import cats.Show
import cats.syntax.show.*
import io.github.nehaev.smarker.Ast.Span
import io.github.nehaev.smarker.Ast.SpanLike
import io.github.nehaev.smarker.Resolver.Context
import scala.annotation.tailrec
import TemplateParser.TemplateReference

// COMMON ERRORS

sealed trait SmarkerError derives CanEqual {

    def rootCause: SmarkerError = this

}

final case class StackSmarkerError(stage: String, cause: SmarkerError) extends SmarkerError {

    override lazy val rootCause = findRootCause(cause)

    @tailrec
    private def findRootCause(e: SmarkerError): SmarkerError = e match {
        case StackSmarkerError(_, cause) => findRootCause(cause)
        case e                           => e
    }

}

final case class SmarkerDuplicateTemplateNameError(
        templateName: String
) extends SmarkerError

final case class SmarkerParseError(
        templateBody: String,
        offset: Int,
        context: String,
        message: String,
) extends SmarkerError {

    def reference: Option[TemplateReference] =
        TemplateParser.findTemplateReference(templateBody, Span(offset, offset))
}

sealed trait SmarkerResolutionError extends SmarkerError {
    def context: Context
    def span: Option[SpanLike]
}

final case class TypeResolutionError(
        message: String,
        expectedType: String,
        actualType: SmarkerType,
        value: Any,
        context: Context,
        span: Option[SpanLike] = None,
) extends SmarkerResolutionError

final case class RequiredParamMissingError(
        directiveName: String,
        paramName: String,
        context: Context,
        span: Option[SpanLike],
) extends SmarkerResolutionError

final case class ScopePathMissingError(
        path: String,
        value: Any,
        context: Context,
        span: Option[SpanLike] = None,
) extends SmarkerResolutionError

final case class TemplateReferenceMissingError(
        templateName: String,
        targetType: SmarkerType,
        value: Any,
        context: Context,
        span: Option[SpanLike],
) extends SmarkerResolutionError

trait SmarkerErrorOps {

    private def showSpan(span: Option[SpanLike]): String = span match {
        case Some(Span(start, _)) => s"\n  at offset $start"
        case _                    => ""
    }

    private def showScope(ctx: Context): String =
        ctx.scope.keys.toList.sorted.mkString("[", ", ", "]")

    given Show[SmarkerError] = Show.show {
        case e: SmarkerDuplicateTemplateNameError =>
            s"""Duplicate template name: "${e.templateName}""""

        case e: SmarkerParseError =>
            e.reference match {
                case Some(ref) =>
                    val caret = " " * ref.spanWithinLine.startOffset + "^"
                    s"""Parse error: ${e.message}
                       |  context: ${e.context}
                       |  at line ${ref.lineNo}, col ${ref.spanWithinLine.startOffset + 1}:
                       |    ${ref.line}
                       |    $caret""".stripMargin
                case None =>
                    s"""Parse error: ${e.message}
                       |  context: ${e.context}
                       |  at offset ${e.offset}""".stripMargin
            }

        case e: StackSmarkerError =>
            val causeLines = e.cause.show.linesIterator.map("  " + _).mkString("\n")
            s"Error in stage '${e.stage}':\n$causeLines"

        case e: TypeResolutionError =>
            s"""Type error: ${e.message}
               |  expected: ${e.expectedType}
               |  actual:   ${e.actualType.show}
               |  scope: ${showScope(e.context)}${showSpan(e.span)}""".stripMargin

        case e: RequiredParamMissingError =>
            s"""Missing required parameter '${e.paramName}' in #${e.directiveName}
               |  scope: ${showScope(e.context)}${showSpan(e.span)}""".stripMargin

        case e: ScopePathMissingError =>
            s"""Undefined variable: '${e.path}'
               |  available in scope: ${showScope(e.context)}${showSpan(e.span)}""".stripMargin

        case e: TemplateReferenceMissingError =>
            val registered = e.context.templateRefs.keys.toList.sorted.mkString("[", ", ", "]")
            s"""No template registered for '${e.templateName}'
               |  type: ${e.targetType.show}
               |  registered templates: $registered${showSpan(e.span)}""".stripMargin
    }

}
