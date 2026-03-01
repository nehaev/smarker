# smarker

A typesafe templating language for Scala 3.

Templates are text that mix raw content with *directives* (block-level control flow) and *interpolations* (inline expressions). Before a template can be rendered, it is checked against a **model** — a structural description of the data you intend to pass in.

## Table of contents

- [Setup](#setup)
- [Quick start](#quick-start)
- [Template syntax](#template-syntax)
  - [Comments](#comments)
  - [Interpolation](#interpolation)
  - [ifDefined](#ifdefined)
  - [list](#list)
  - [block](#block)
- [Supported types](#supported-types)
  - [Primitives](#primitives)
  - [Collections](#collections)
  - [Object types](#object-types)
- [Whitespace control](#whitespace-control)

## Setup

```scala
libraryDependencies += "io.github.nehaev" %% "smarker" % "<version>"
```

## Quick start

```scala
import io.github.nehaev.smarker.*

case class Ingredient(name: String)
case class Product(name: String, price: Int, ingredients: List[Ingredient])
case class Market(products: List[Product])

val templateSet = templates(
    template[Ingredient]("[=name]"),
    template[Product]("[=name]: $[=price] ([#list items=ingredients sep=\", \" /])"),
    template[Market]("[#list items=products sep=\", \" /]"),
).getOrElse(sys.error("template parse error"))

val market = Market(List(
    Product("Apple Pie", 3, List(Ingredient("Flour"), Ingredient("Sugar"))),
    Product("Banana",    5, List(Ingredient("Banana"))),
))

templateSet.render(market)
// Right("Apple Pie: $3 (Flour, Sugar), Banana: $5 (Banana)")
```

## Template syntax

The template expression syntax uses square brackets: `[=expr]` for interpolations and `[#directive p1=v1 p2=v2]...[/#directive]` for block directives.

### Comments

Comments are stripped from the output and never rendered.

```
[#-- This is a comment. It will not appear in the output. --]
```

### Interpolation

Embed the value of an expression into the output using `[=...]`. If the type of the expression is primitive, the value is implicitly converted to string. If the type of the expression is `class`, smarker looks for a registered template for that class and renders it.

```scala
case class Address(street: String, city: String)
case class User(name: String, address: Address)
```

```
Hello, [=name]! Welcome to [=address.city].
```

```
// User("Alice", Address("123 Main St", "Anytown"))
Hello, Alice! Welcome to Anytown.
```

### ifDefined

`[#ifDefined]` guards access to a value that may be absent. It renders its body when the value is present, otherwise emits the `alt` string. The `as` parameter binds the unwrapped value inside the body.

```scala
case class User(name: String, age: Option[Int], permissions: Map[String, Boolean])
```

Optional field — present:
```
[#ifDefined value=age as="a"]age=[=a][/#ifDefined]
// User(age = Some(30)) → "age=30"
```

Optional field — absent:
```
[#ifDefined value=age alt="unknown"][=_][/#ifDefined]
// User(age = None) → "unknown"
```

Map key — present:
```
[#ifDefined value=permissions.admin as="v"]has-admin=[=v][/#ifDefined]
// permissions = Map("admin" -> true) → "has-admin=true"
```

Map key — absent:
```
[#ifDefined value=permissions.superuser alt="no-perm"][=_][/#ifDefined]
// permissions = Map("admin" -> true) → "no-perm"
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

### list

Iterate over a sequence with `[#list]`.

```scala
case class User(scores: List[Int])
// User(scores = List(95, 87, 42))
```

Default separator:
```
[#list items=scores][=_][/#list]
// "95, 87, 42"
```

Custom separator:
```
[#list items=scores sep="|"][=_][/#list]
// "95|87|42"
```

With `start` and `end`:
```
[#list items=scores start="[" end="]"][=_][/#list]
// "[95, 87, 42]"
```

Class items with field access:
```scala
case class Item(label: String, value: Int)
case class Root(items: List[Item])
// Root(items = List(Item("a", 1), Item("b", 2)))
```
```
[#list items=items as="it"][=it.label]=[=it.value][/#list]
// "a=1, b=2"
```

When no body is given (`[#list items=xs /]`), smarker looks up the registered template for the element type and uses it to render each item.

| Param   | Type           | Default | Description                                   |
|---------|----------------|---------|-----------------------------------------------|
| `items` | `list[T]`      |         | Collection to iterate over (required)         |
| `as`    | string         | `"_"`   | Name of the loop variable inside the body     |
| `sep`   | string         | `", "`  | String inserted between consecutive elements  |
| `start` | string         | `""`    | String prepended before the first element     |
| `end`   | string         | `""`    | String appended after the last element        |

The loop variable is scoped to the list body. Its type is inferred from the element type declared in the model, so field access is fully type-checked.

### block

`[#block]` explicitly controls indentation of its content. Because smarker strips leading whitespace from text segments, source indentation has no effect on output. Use `[#block]` whenever the rendered output needs to be indented.

Default indentation (4 spaces):
```
line1
[#block]line2[/#block]
// "line1\n    line2"
```

Nested blocks accumulate indentation:
```
[#block][=name]
[#block][=name][/#block][/#block]
// "    Alice\n        Alice"
```

Custom `identChar` and `identSize`:
```
[#block identChar="-" identSize=2]X[/#block]
// "--X"
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

| Type    | Description                                                                          |
|---------|--------------------------------------------------------------------------------------|
| `class` | Named object type with a fixed set of typed fields, like a Scala case class          |
| `dyn`   | Dynamically structured value with no statically known fields; field access is permitted but not type-checked |

## Whitespace control

Smarker strips leading whitespace (indentation) from any text segment or expression output. To produce indented output use the `[#block]` directive.

Trailing whitespace (including the line break) is stripped from lines that contain only directives or comments. If a directive is followed by multiple consecutive line breaks, only the first is stripped.
