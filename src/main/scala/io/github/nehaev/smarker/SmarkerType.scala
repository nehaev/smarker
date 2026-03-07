package io.github.nehaev.smarker

import cats.syntax.show.*
import cats.Show

enum SmarkerType derives CanEqual {

    // Primitive types always rendered as toString
    case String
    case Int
    case Bool

    // Collection types
    case List(itemType: SmarkerType)
    case Map(valueType: SmarkerType)
    case Opt(valueType: SmarkerType)

    // Object types
    case Class(name: String, fields: () => collection.Map[String, SmarkerType]) // lazy fields to allow recursive types
    case Dyn

    case Nothing
}

object SmarkerType {
    given Show[SmarkerType] = Show.show {
        case String      => "string"
        case Int         => "int"
        case Bool        => "bool"
        case List(t)     => s"list[${t.show}]"
        case Map(t)      => s"map[${t.show}]"
        case Opt(t)      => s"opt[${t.show}]"
        case Class(n, _) => n
        case Dyn         => "dyn"
        case Nothing     => "nothing"
    }
}
