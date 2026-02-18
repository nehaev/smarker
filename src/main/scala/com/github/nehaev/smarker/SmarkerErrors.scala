package com.github.nehaev.smarker

import scala.annotation.tailrec

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
