package com.github.nehaev.smarker

object TemplateParser {
    import cats.parse.{Parser => P}
    import cats.parse.{Parser0 => P0}
    import cats.parse.Rfc5234.digit
    import Ast._

    given Conversion[Char, P[Unit]] = P.char(_)
    given Conversion[String, P[Unit]] = P.string(_)
    extension (p: Char => Boolean) {
        infix def ||(po: Char => Boolean): Char => Boolean = c => p(c) || po(c)
    }
    extension [A](p: P[A]) {
        def dirSpaced: P[A] = p.soft.surroundedBy(directiveWs)
    }
    extension [A](p: P0[A]) {
        def dirSpaced: P0[A] = p.surroundedBy(directiveWs)
    }

    // WHITESPACING
    val nl = P.charIn('\n', '\r').void
    val wsp = P.charIn(' ', '\t').void
    val directiveWs = (wsp | nl).rep0.void

    // LITERALS
    val boolLit = ("true" | "false").string
        .map(s => BoolLiteral(s.toBoolean))
    val stringLit = {
        val stringBreak = P.charIn('"', '\n', '\\')
        val escape = '\\' *> P.charIn('\\', '"', 'r', 'n', 't').map {
            case 'r'   => "\r"
            case 'n'   => "\n"
            case 't'   => "\t"
            case other => other.toString
        }
        val stringChars = (P.until(stringBreak) | escape).rep0.map(_.mkString)
        ('"' *> stringChars <* '"').map(StringLiteral.apply)
    }
    val intLit = ('-'.?.with1 ~ digit.rep).string.map(s => IntLiteral(s.toInt))

    // EXPRESSIONS
    val id = {
        val lowerLetter = (c: Char) => c >= 'a' && c <= 'z'
        val upperLetter = (c: Char) => c >= 'A' && c <= 'Z'
        val digit = (c: Char) => c >= '0' && c <= '9'
        val underscore = (c: Char) => c == '_'
        val underscoreOrDash = (c: Char) => c == '_' || c == '-'
        val keywords = Set("true", "false")
        val first = P.charWhere(lowerLetter || upperLetter || underscore)
        val rest = P.charWhere(lowerLetter || upperLetter || digit || underscoreOrDash).rep0
        (first ~ rest).string.filter(!keywords.apply(_))
    }
    val ident = id.map(Ident.apply)
    val select = (ident ~ ('.' *> id).rep.map(_.toList)).map {
        case (init, head :: tail) => tail.foldLeft(Select(init, head)) { case (acc, field) => Select(acc, field) }
        case _                    => ???
    }
    val expr: P[Expr] = (boolLit | stringLit | intLit | select.backtrack | ident)
        .withContext("expression")

    // TEMPLATE STRUCTURE
    val rawText = {
        val rawBreak = "[" ~ ("#" | "=" | "/#")
        val line = P.until(rawBreak | nl)
        (nl.as(RawNewLine) | line.map(RawString.apply)).rep
            .withContext("rawText")
            .map(elements => RawText(elements.toList))
    }

    val comment = ("[#--" *> P.until0("--]") <* "--]").withContext("comment").map(Comment.apply)
    val interpolation = ("[=" *> expr.dirSpaced <* "]").withContext("interpolation").map(Interpolation.apply)
    val directiveCall: P[DirectiveCall] = {
        val arg = (id <* "=".dirSpaced) ~ expr
        val args = arg.repSep0(directiveWs)
            .withContext("dirCall.args")
            .map(_.toMap)
        val dirStart = "[#" *> id ~ args.dirSpaced
        val body = P.defer(templateElement).rep0
            .withContext("dirCall.body")
            .map(tes => if (tes.isEmpty) None else Some(tes))
        val dirShortEnd = "/]".as(None: Option[List[TemplateElement]])
        def dirEnd(name: String): P[Unit] = "[/#" *> name <* "]"

        dirStart
            .flatMap { case (name, args) =>
                (dirShortEnd | ("]" *> body <* dirEnd(name))).map(b => DirectiveCall(name, args, b))
            }
            .withContext("dirCall")
    }

    val templateElement = (interpolation | comment | directiveCall | rawText)
        .withContext("templateElement")
    val templateBody = templateElement.rep0.map(elements => TemplateBody(elements.toList))
}
