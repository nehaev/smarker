package com.github.nehaev.smarker

import cats.syntax.show.toShow
import utest.*
import Ast.*
import TemplateParser.*

object TemplateParserTests extends TestSuite {

    val tests = Tests {
        test("BoolLiteral") {
            test("true") {
                val res = boolLit.parseAll("true").left.map(_.show)
                assert(res == Right(BoolLiteral(true)))
            }
            test("false") {
                val res = boolLit.parseAll("false").left.map(_.show)
                assert(res == Right(BoolLiteral(false)))
            }
            test("wrong case fails") {
                assert(boolLit.parseAll("True").isLeft)
            }
            test("extra chars fails") {
                assert(boolLit.parseAll("true1").isLeft)
            }
        }
        test("IntLiteral") {
            test("zero") {
                val res = intLit.parseAll("0").left.map(_.show)
                assert(res == Right(IntLiteral(0)))
            }
            test("positive") {
                val res = intLit.parseAll("42").left.map(_.show)
                assert(res == Right(IntLiteral(42)))
            }
            test("negative") {
                val res = intLit.parseAll("-7").left.map(_.show)
                assert(res == Right(IntLiteral(-7)))
            }
            test("multi-digit") {
                val res = intLit.parseAll("1234").left.map(_.show)
                assert(res == Right(IntLiteral(1234)))
            }
            test("bare minus fails") {
                assert(intLit.parseAll("-").isLeft)
            }
            test("letters fail") {
                assert(intLit.parseAll("abc").isLeft)
            }
        }
        test("StringLiteral") {
            test("empty") {
                val res = stringLit.parseAll("\"\"").left.map(_.show)
                assert(res == Right(StringLiteral("")))
            }
            test("simple") {
                val res = stringLit.parseAll("\"hello\"").left.map(_.show)
                assert(res == Right(StringLiteral("hello")))
            }
            test("newline escape") {
                val res = stringLit.parseAll("\"hello\\nworld\"").left.map(_.show)
                assert(res == Right(StringLiteral("hello\nworld")))
            }
            test("tab escape") {
                val res = stringLit.parseAll("\"tab\\there\"").left.map(_.show)
                assert(res == Right(StringLiteral("tab\there")))
            }
            test("quote escape") {
                val res = stringLit.parseAll("\"say \\\"hi\\\"\"").left.map(_.show)
                assert(res == Right(StringLiteral("say \"hi\"")))
            }
            test("backslash escape") {
                val res = stringLit.parseAll("\"back\\\\slash\"").left.map(_.show)
                assert(res == Right(StringLiteral("back\\slash")))
            }
            test("unclosed quote fails") {
                assert(stringLit.parseAll("\"unclosed").isLeft)
            }
        }
        test("Ident") {
            test("simple lowercase") {
                val res = ident.parseAll("foo").left.map(_.show)
                assert(res == Right(Ident("foo")))
            }
            test("uppercase") {
                val res = ident.parseAll("Bar").left.map(_.show)
                assert(res == Right(Ident("Bar")))
            }
            test("mixed case") {
                val res = ident.parseAll("fooBar").left.map(_.show)
                assert(res == Right(Ident("fooBar")))
            }
            test("with underscore") {
                val res = ident.parseAll("foo_bar").left.map(_.show)
                assert(res == Right(Ident("foo_bar")))
            }
            test("with dash") {
                val res = ident.parseAll("foo-bar").left.map(_.show)
                assert(res == Right(Ident("foo-bar")))
            }
            test("with trailing digit") {
                val res = ident.parseAll("foo1").left.map(_.show)
                assert(res == Right(Ident("foo1")))
            }
            test("leading underscore") {
                val res = ident.parseAll("_foo").left.map(_.show)
                assert(res == Right(Ident("_foo")))
            }
            test("leading digit fails") {
                assert(ident.parseAll("1foo").isLeft)
            }
            test("keyword true fails") {
                assert(ident.parseAll("true").isLeft)
            }
            test("keyword false fails") {
                assert(ident.parseAll("false").isLeft)
            }
            test("empty fails") {
                assert(ident.parseAll("").isLeft)
            }
        }
        test("Select") {
            test("two parts") {
                val res = select.parseAll("a.b").left.map(_.show)
                assert(res == Right(Select(Ident("a"), "b")))
            }
            test("three parts") {
                val res = select.parseAll("a.b.c").left.map(_.show)
                assert(res == Right(Select(Select(Ident("a"), "b"), "c")))
            }
            test("meaningful names") {
                val res = select.parseAll("user.name").left.map(_.show)
                assert(res == Right(Select(Ident("user"), "name")))
            }
            test("plain ident fails") {
                assert(select.parseAll("a").isLeft)
            }
            test("leading dot fails") {
                assert(select.parseAll(".b").isLeft)
            }
            test("trailing dot fails") {
                assert(select.parseAll("a.").isLeft)
            }
        }
        test("Expr") {
            test("routes to boolLit: true") {
                val res = expr.parseAll("true").left.map(_.show)
                assert(res == Right(BoolLiteral(true)))
            }
            test("routes to boolLit: false") {
                val res = expr.parseAll("false").left.map(_.show)
                assert(res == Right(BoolLiteral(false)))
            }
            test("routes to stringLit") {
                val res = expr.parseAll("\"hello\"").left.map(_.show)
                assert(res == Right(StringLiteral("hello")))
            }
            test("routes to intLit positive") {
                val res = expr.parseAll("42").left.map(_.show)
                assert(res == Right(IntLiteral(42)))
            }
            test("routes to intLit negative") {
                val res = expr.parseAll("-5").left.map(_.show)
                assert(res == Right(IntLiteral(-5)))
            }
            test("routes to select") {
                val res = expr.parseAll("a.b").left.map(_.show)
                assert(res == Right(Select(Ident("a"), "b")))
            }
            test("routes to select chain") {
                val res = expr.parseAll("a.b.c").left.map(_.show)
                assert(res == Right(Select(Select(Ident("a"), "b"), "c")))
            }
            test("routes to ident") {
                val res = expr.parseAll("foo").left.map(_.show)
                assert(res == Right(Ident("foo")))
            }
            test("boolLit takes priority over ident") {
                // without boolLit before ident, "true" would be Ident("true")
                assert(expr.parseAll("true") == Right(BoolLiteral(true)))
            }
            test("select takes priority over ident") {
                // without select before ident, "foo.bar" would parse only "foo" as Ident
                assert(expr.parseAll("foo.bar") == Right(Select(Ident("foo"), "bar")))
            }
            test("empty fails") {
                assert(expr.parseAll("").isLeft)
            }
        }
        test("Comment") {
            test("simple") {
                val res = comment.parseAll("[#-- hello --]").left.map(_.show)
                assert(res == Right(Comment(" hello ")))
            }
            test("empty body") {
                val res = comment.parseAll("[#----]").left.map(_.show)
                assert(res == Right(Comment("")))
            }
            test("multiline body") {
                val res = comment.parseAll("[#-- line1\nline2 --]").left.map(_.show)
                assert(res == Right(Comment(" line1\nline2 ")))
            }
            test("double dash inside body") {
                // "--" alone is not the closing marker, only "--]" is
                val res = comment.parseAll("[#-- a -- b --]").left.map(_.show)
                assert(res == Right(Comment(" a -- b ")))
            }
            test("unclosed fails") {
                assert(comment.parseAll("[#-- no end").isLeft)
            }
            test("missing open fails") {
                assert(comment.parseAll("-- not a comment --]").isLeft)
            }
        }
        test("Interpolation") {
            test("ident") {
                val res = interpolation.parseAll("[=foo]").left.map(_.show)
                assert(res == Right(Interpolation(Ident("foo"))))
            }
            test("select") {
                val res = interpolation.parseAll("[=user.name]").left.map(_.show)
                assert(res == Right(Interpolation(Select(Ident("user"), "name"))))
            }
            test("bool literal") {
                val res = interpolation.parseAll("[=true]").left.map(_.show)
                assert(res == Right(Interpolation(BoolLiteral(true))))
            }
            test("int literal") {
                val res = interpolation.parseAll("[=42]").left.map(_.show)
                assert(res == Right(Interpolation(IntLiteral(42))))
            }
            test("surrounding spaces trimmed") {
                val res = interpolation.parseAll("[=  foo  ]").left.map(_.show)
                assert(res == Right(Interpolation(Ident("foo"))))
            }
            test("surrounding newlines trimmed") {
                val res = interpolation.parseAll("[=\nfoo\n]").left.map(_.show)
                assert(res == Right(Interpolation(Ident("foo"))))
            }
            test("empty expr fails") {
                assert(interpolation.parseAll("[=]").isLeft)
            }
            test("unclosed bracket fails") {
                assert(interpolation.parseAll("[=foo").isLeft)
            }
        }
        test("RawText") {
            test("plain text") {
                val res = rawText.parseAll("hello world").left.map(_.show)
                assert(res == Right(RawText(List(RawString("hello world")))))
            }
            test("bare bracket not followed by special char") {
                // "[" only breaks when followed by #, =, /# or --
                val res = rawText.parseAll("foo[bar]baz").left.map(_.show)
                assert(res == Right(RawText(List(RawString("foo[bar]baz")))))
            }
            test("stops before [#") {
                val res = rawText.parse("hello[#list]").left.map(_.show)
                assert(res == Right(("[#list]", RawText(List(RawString("hello"))))))
            }
            test("stops before [=") {
                val res = rawText.parse("hello[=expr]").left.map(_.show)
                assert(res == Right(("[=expr]", RawText(List(RawString("hello"))))))
            }
            test("stops before [/#") {
                val res = rawText.parse("text[/#close]").left.map(_.show)
                assert(res == Right(("[/#close]", RawText(List(RawString("text"))))))
            }
            test("consumes multiple lines") {
                val res = rawText.parseAll("line1\n\nmore").left.map(_.show)
                assert(res == Right(RawText(List(RawString("line1"), RawNewLine, RawNewLine, RawString("more")))))
            }
        }
        test("DirectiveCall") {
            test("self-closing no args") {
                val res = directiveCall.parseAll("[#list /]").left.map(_.show)
                assert(res == Right(DirectiveCall("list", Map(), None)))
            }
            test("self-closing with ident arg") {
                val res = directiveCall.parseAll("[#list items=products /]").left.map(_.show)
                assert(res == Right(DirectiveCall("list", Map("items" -> Ident("products")), None)))
            }
            test("self-closing with string arg") {
                val res = directiveCall.parseAll("[#list sep=\"\\n\" /]").left.map(_.show)
                assert(res == Right(DirectiveCall("list", Map("sep" -> StringLiteral("\n")), None)))
            }
            test("self-closing with select arg") {
                val res = directiveCall.parseAll("[#list items=user.products /]").left.map(_.show)
                assert(res == Right(DirectiveCall("list", Map("items" -> Select(Ident("user"), "products")), None)))
            }
            test("self-closing with multiple args") {
                val res = directiveCall.parseAll("[#list items=products sep=\", \" /]").left.map(_.show)
                assert(res == Right(DirectiveCall("list", Map("items" -> Ident("products"), "sep" -> StringLiteral(", ")), None)))
            }
            test("block empty body") {
                val res = directiveCall.parseAll("[#block][/#block]").left.map(_.show)
                assert(res == Right(DirectiveCall("block", Map(), None)))
            }
            test("block with raw text body") {
                val res = directiveCall.parseAll("[#block]hello[/#block]").left.map(_.show)
                assert(res == Right(DirectiveCall("block", Map(), Some(List(RawText(List(RawString("hello"))))))))
            }
            test("block with interpolation body") {
                val res = directiveCall.parseAll("[#block][=name][/#block]").left.map(_.show)
                assert(res == Right(DirectiveCall("block", Map(), Some(List(Interpolation(Ident("name")))))))
            }
            test("block with nested directive") {
                val res = directiveCall.parseAll("[#block][#list /][/#block]").left.map(_.show)
                assert(res == Right(DirectiveCall("block", Map(), Some(List(DirectiveCall("list", Map(), None))))))
            }
            test("block with args") {
                val res = directiveCall
                    .parseAll("[#notEmpty value=user.nickname fallback=\"(none)\"][=user.nickname][/#notEmpty]")
                    .left
                    .map(_.show)
                assert(
                    res == Right(
                        DirectiveCall(
                            "notEmpty",
                            Map("value" -> Select(Ident("user"), "nickname"), "fallback" -> StringLiteral("(none)")),
                            Some(List(Interpolation(Select(Ident("user"), "nickname")))),
                        )
                    )
                )
            }
            test("mismatched close tag fails") {
                assert(directiveCall.parseAll("[#foo][/#bar]").isLeft)
            }
            test("unclosed block fails") {
                assert(directiveCall.parseAll("[#foo]").isLeft)
            }
            test("missing open bracket fails") {
                assert(directiveCall.parseAll("#foo /]").isLeft)
            }
        }
        test("TemplateBody") {
            test("raw text interleaved with interpolations") {
                // e.g. "Recipe: <name>, serves <n>"
                val input = "Recipe: [=recipe.name]\nServes: [=recipe.servings]"
                val res = templateBody.parseAll(input).left.map(_.show)
                assert(
                    res == Right(
                        TemplateBody(
                            List(
                                RawText(List(RawString("Recipe: "))),
                                Interpolation(Select(Ident("recipe"), "name")),
                                RawText(List(RawNewLine, RawString("Serves: "))),
                                Interpolation(Select(Ident("recipe"), "servings")),
                            )
                        )
                    )
                )
            }
            test("comment followed by interpolations") {
                // comment is stripped; remaining interpolations appear in sequence
                val input = "[#-- recipe header --][=recipe.name]by[=recipe.author]"
                val res = templateBody.parseAll(input).left.map(_.show)
                assert(
                    res == Right(
                        TemplateBody(
                            List(
                                Comment(" recipe header "),
                                Interpolation(Select(Ident("recipe"), "name")),
                                RawText(List(RawString("by"))),
                                Interpolation(Select(Ident("recipe"), "author")),
                            )
                        )
                    )
                )
            }
            test("notEmpty guard followed by self-closing list") {
                // render intro if present, then always render the ingredient list
                val input = "[#notEmpty value=recipe.intro][=recipe.intro][/#notEmpty][#list items=recipe.steps sep=\",\" /]"
                val res = templateBody.parseAll(input).left.map(_.show)
                assert(
                    res == Right(
                        TemplateBody(
                            List(
                                DirectiveCall(
                                    "notEmpty",
                                    Map("value" -> Select(Ident("recipe"), "intro")),
                                    Some(List(Interpolation(Select(Ident("recipe"), "intro")))),
                                ),
                                DirectiveCall("list", Map("items" -> Select(Ident("recipe"), "steps"), "sep" -> StringLiteral(",")), None),
                            )
                        )
                    )
                )
            }
            test("block with multiple body elements") {
                // indented section containing a label and a nested self-closing list
                val input = "[#block][=name][#list items=ingredients /][/#block]"
                val res = templateBody.parseAll(input).left.map(_.show)
                assert(
                    res == Right(
                        TemplateBody(
                            List(
                                DirectiveCall(
                                    "block",
                                    Map(),
                                    Some(
                                        List(
                                            Interpolation(Ident("name")),
                                            DirectiveCall("list", Map("items" -> Ident("ingredients")), None),
                                        )
                                    ),
                                )
                            )
                        )
                    )
                )
            }
            test("consecutive self-closing directives") {
                // list categories, then list products
                val input = "[#list items=categories /][#list items=products /]"
                val res = templateBody.parseAll(input).left.map(_.show)
                assert(
                    res == Right(
                        TemplateBody(
                            List(
                                DirectiveCall("list", Map("items" -> Ident("categories")), None),
                                DirectiveCall("list", Map("items" -> Ident("products")), None),
                            )
                        )
                    )
                )
            }
            test("deeply nested block with notEmpty inside") {
                // optional chef name inside an indented block
                val input = "[#block][#notEmpty value=chef][=chef][/#notEmpty][/#block]"
                val res = templateBody.parseAll(input).left.map(_.show)
                assert(
                    res == Right(
                        TemplateBody(
                            List(
                                DirectiveCall(
                                    "block",
                                    Map(),
                                    Some(
                                        List(
                                            DirectiveCall(
                                                "notEmpty",
                                                Map("value" -> Ident("chef")),
                                                Some(List(Interpolation(Ident("chef")))),
                                            )
                                        )
                                    ),
                                )
                            )
                        )
                    )
                )
            }
            test("full recipe card: header text, optional intro, ingredient list") {
                val input =
                    "Recipe:[=recipe.name]" +
                        "[#notEmpty value=recipe.intro fallback=\"(no intro)\"][=recipe.intro][/#notEmpty]" +
                        "[#list items=recipe.ingredients sep=\",\" /]"
                val res = templateBody.parseAll(input).left.map(_.show)
                assert(
                    res == Right(
                        TemplateBody(
                            List(
                                RawText(List(RawString("Recipe:"))),
                                Interpolation(Select(Ident("recipe"), "name")),
                                DirectiveCall(
                                    "notEmpty",
                                    Map("value" -> Select(Ident("recipe"), "intro"), "fallback" -> StringLiteral("(no intro)")),
                                    Some(List(Interpolation(Select(Ident("recipe"), "intro")))),
                                ),
                                DirectiveCall(
                                    "list",
                                    Map("items" -> Select(Ident("recipe"), "ingredients"), "sep" -> StringLiteral(",")),
                                    None,
                                ),
                            )
                        )
                    )
                )
            }
        }
    }

}
