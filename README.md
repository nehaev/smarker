# smarker

A minimalistic typesafe templating language for Scala 3.
It has [freemarker](https://github.com/apache/freemarker)-like syntax.
Smarker's main purpose is to render well-formed type-annotated structures (called *models*) into text.

## Table of contents

- [Quick start](#quick-start)
- [Template syntax](#template-syntax)
  - [Comments](#comments)
  - [Interpolation](#interpolation)
  - [ifDefined](#ifdefined)
  - [if](#if)
  - [list](#list)
  - [block](#block)
- [Supported types](#supported-types)
  - [Primitives](#primitives)
  - [Collections](#collections)
  - [Object types](#object-types)
- [Whitespace control](#whitespace-control)

## Quick start

Add the following dependency to your project:

```scala
libraryDependencies += "io.github.nehaev" %% "smarker" % "<version>"
```

Define models, templates, and render:

```scala
import io.github.nehaev.smarker.*

case class Item(
    name: String,
    qty: Int,
    inStock: Boolean
)

case class Order(
    id: String,
    customer: String,
    items: List[Item],
    promoCode: Option[String],
    urgent: Boolean,
)

val templateSet = templates(
    template[Order]("""
        |[#-- Order confirmation --]
        |[#if cond=urgent]
        |    URGENT
        |[/#if]
        |Order #[=id] for [=customer]
        |[#ifDefined value=promoCode as="code"]
        |    Promo: [=code]
        |[/#ifDefined]
        |Items:
        |[#block]
        |    [#list items=items sep="\n" /]
        |[/#block]""".trim.stripMargin
    ),
    template[Item]("""[=name] x[=qty][#if cond=inStock] (in stock)[/#if]""")
)

val order = Order(
    id = "1042",
    customer = "Alice",
    items = List(
        Item(name = "Widget", qty = 2, inStock = true),
        Item(name = "Gadget", qty = 1, inStock = false),
    ),
    promoCode = Some("SAVE10"),
    urgent = true,
)

println(templateSet.flatMap(_.render(order)))
```

This should print the following output:

```
URGENT
Order #1042 for Alice
Promo: SAVE10
Items:
    Widget x2 (in stock)
    Gadget x1
```

## Template syntax

Smarker templates are text that mix raw content with *directives* (block-level control flow) and *interpolations* (inline expressions).

The template expression syntax uses square brackets: `[=expr]` for interpolations and `[#directive p1=v1 p2=v2]...[/#directive]` for block directives.

### Comments

Comments are stripped from the output and never rendered.

```
[#-- This is a comment. It will not appear in the output. --]
```

### Interpolation

Embed the value of an expression into the output using `[=...]`.
If the type of the expression is primitive, the value is implicitly converted to string.
If the type of the expression is `class`, smarker looks for a registered template for that class and renders it.

```
Hello, [=name]! Welcome to [=address.city].
```

```
// User(name = "Alice", address = Address(street = "123 Main St", city = "Anytown"))
Hello, Alice! Welcome to Anytown.
```

### ifDefined

`[#ifDefined]` guards access to a value that may be absent.
It renders its body when the value is present, otherwise emits the `alt` string.
The `as` parameter binds the unwrapped value inside the body.

```
[#ifDefined value=age alt="unknown" as="a"]
    age=[=a]
[/#ifDefined]
```

```
// User(name = "Alice", age = Some(30))
age=30

// User(name = "Alice", age = None)
unknown
```

| Param   | Type   | Default | Description                                                     |
|---------|--------|---------|-----------------------------------------------------------------|
| `value` | `T`    |         | The value to check for presence (required)                      |
| `alt`   | string | `""`    | String to emit when the value is absent                         |
| `as`    | string | `"_"`   | Name of the variable bound to the unwrapped value in the body   |

Absence is defined per type:
- `opt[T]` — absent when the value is `None`; body receives the unwrapped `T`
- `map.key` — absent when the key does not exist in the map; body receives the value at that key
- `class.field` — absent when the field is `None` (i.e. the field is `opt[T]`); body receives the unwrapped `T`
- `dyn.field` — absent when the field is missing or null at runtime

### if

`[#if]` renders its body only when a boolean condition is true.
When the condition is false, nothing is emitted.
The body is required; `[#if]` has no self-closing form.

```
[#if cond=active]
    Name: [=name]
[/#if]
```

```
// User(name = "Alice", active = true)
Name: Alice

// User(name = "Alice", active = false)

```

| Param  | Type   | Default | Description                          |
|--------|--------|---------|--------------------------------------|
| `cond` | `bool` |         | The condition to evaluate (required) |

### list

Iterate over a sequence with `[#list]`.
When no body is given (`[#list items=xs /]`), smarker looks up the registered template for the element type and uses it to render each item.

```
[#list items=items sep="\n" as="item"]
    - [=item.name]: $[=item.price]
[/#list]
```

```
// Cart(items = List(Item(name = "Coffee", price = 3), Item(name = "Muffin", price = 2)))
- Coffee: $3
- Muffin: $2
```

| Param   | Type           | Default | Description                                   |
|---------|----------------|---------|-----------------------------------------------|
| `items` | `list[T]`      |         | Collection to iterate over (required)         |
| `as`    | string         | `"_"`   | Name of the loop variable inside the body     |
| `sep`   | string         | `", "`  | String inserted between consecutive elements  |
| `start` | string         | `""`    | String prepended before the first element     |
| `end`   | string         | `""`    | String appended after the last element        |

The loop variable is scoped to the list body.
Its type is inferred from the element type declared in the model, so field access is fully type-checked.

### block

`[#block]` explicitly controls indentation of its content.
Because smarker strips leading whitespace from text segments, source indentation has no effect on output.
Use `[#block]` whenever the rendered output needs to be indented.

```
database:
[#block]
    host: [=host]
    port: [=port]
    name: [=name]
[/#block]
```

```
// Database(host = "localhost", port = 5432, name = "mydb")
database:
    host: localhost
    port: 5432
    name: mydb
```

| Param       | Type   | Default | Description                                              |
|-------------|--------|---------|----------------------------------------------------------|
| `identChar` | string | `" "`   | The character repeated to form one level of indentation  |
| `identSize` | int    | `4`     | Number of `identChar` repetitions per indentation level  |
| `start`     | string | `""`    | String prepended before the block content                |
| `end`       | string | `""`    | String appended after the block content                  |

## Supported types

### Primitives

| Type     | Description                             |
|----------|-----------------------------------------|
| `string` | UTF-8 text value                        |
| `int`    | 32-bit signed integer                   |
| `bool`   | Boolean value (`true` or `false`)       |

### Collections

| Type      | Description                                                                   |
|-----------|-------------------------------------------------------------------------------|
| `list[T]` | Ordered sequence of elements of type `T`                                      |
| `map[T]`  | String-keyed map with values of type `T`; keys accessed with dot notation     |
| `opt[T]`  | A value of type `T` that may be absent; must be guarded with `#ifDefined`     |

### Object types

| Type    | Description                                                                                                 |
|---------|-------------------------------------------------------------------------------------------------------------|
| `class` | Named object type with a fixed set of typed fields, like a Scala case class                                 |
| `dyn`   | Dynamically structured value with no statically known fields; field access is permitted but not type-checked |

## Whitespace control (WIP)

Smarker strips leading whitespace (indentation) from any text segment or expression output.
To produce indented output use the `[#block]` directive.

Trailing whitespace (including the line break) is stripped from lines that contain only directives or comments.
If a directive is followed by multiple consecutive line breaks, only the first is stripped.
