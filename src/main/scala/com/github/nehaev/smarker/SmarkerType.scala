package com.github.nehaev.smarker

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
