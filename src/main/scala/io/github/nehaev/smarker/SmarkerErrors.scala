package io.github.nehaev.smarker

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
        targetType: SmarkerType,
        value: Any,
        context: Context,
        span: Option[SpanLike],
) extends SmarkerResolutionError
