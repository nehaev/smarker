package io.github.nehaev.smarker

import utest._

object TemplateSetTests extends TestSuite {

    val tests = Tests {

        case class Ingredient(name: String)
        case class Product(name: String, price: Int, ingredients: List[Ingredient])
        case class Market(products: List[Product])

        test("smarker.template") {
            test("valid source returns correct typeName") {
                val ct = template[Ingredient]("[=name]")
                assert(ct.isRight)
                assert(ct.toOption.get.name == "Ingredient")
            }
            test("invalid source returns SmarkerParseError") {
                val ct = template[Ingredient]("[=")
                assert(ct.isLeft)
                assert(ct.left.toOption.get.isInstanceOf[SmarkerParseError])
            }
            test("explicit name overload uses provided name") {
                val ct = template[Ingredient]("ing", "[=name]")
                assert(ct.isRight)
                assert(ct.toOption.get.name == "ing")
            }
        }

        test("smarker.templates") {
            test("all valid works") {
                val ts = templates(
                    template[Ingredient]("[=name]"),
                    template[Product]("[=name]"),
                )
                assert(ts.isRight)
            }
            test("duplicate template names fail") {
                val ts = templates(
                    template[Ingredient]("[=name]"),
                    template[Ingredient]("[=name]"),
                )
                assert(ts.isLeft)
                val err = ts.left.toOption.get
                assert(err.isInstanceOf[SmarkerDuplicateTemplateNameError])
                assert(err.asInstanceOf[SmarkerDuplicateTemplateNameError].templateName == "Ingredient")
            }
            test("duplicate explicit names returns fail") {
                val ts = templates(
                    template[Ingredient]("shared", "[=name]"),
                    template[Product]("shared", "[=name]"),
                )
                assert(ts.isLeft)
                val err = ts.left.toOption.get
                assert(err.isInstanceOf[SmarkerDuplicateTemplateNameError])
                assert(err.asInstanceOf[SmarkerDuplicateTemplateNameError].templateName == "shared")
            }
        }

        test("TemplateSet.render") {
            test("registered template renders correctly") {
                val ts = templates(template[Ingredient]("[=name]")).toOption.get
                val result = ts.render(Ingredient("Flour"))
                assert(result == Right("Flour"))
            }
            test("unregistered type returns TemplateReferenceMissingError") {
                val ts = templates(template[Ingredient]("[=name]")).toOption.get
                val result = ts.render(Product("Apple", 3, List()))
                assert(result.isLeft)
                assert(result.left.toOption.get.isInstanceOf[TemplateReferenceMissingError])
            }
            test("all templates in the set are available as templateRefs for body-less directives") {
                val ts = templates(
                    template[Ingredient]("[=name]"),
                    template[Product]("[#list items=ingredients sep=\", \" /]"),
                ).toOption.get
                val product = Product("Apple Pie", 3, List(Ingredient("Flour"), Ingredient("Sugar")))
                val result = ts.render(product)
                assert(result == Right("Flour, Sugar"))
            }
            test("full example: three-level nested body-less list across templates") {
                val ts = templates(
                    template[Ingredient]("[=name]"),
                    template[Product]("[=name]: $[=price] ([#list items=ingredients sep=\", \" /])"),
                    template[Market]("[#list items=products sep=\", \" /]"),
                ).toOption.get
                val market = Market(
                    List(
                        Product("Apple Pie", 3, List(Ingredient("Flour"), Ingredient("Sugar"))),
                        Product("Banana", 5, List(Ingredient("Banana"))),
                    )
                )
                val result = ts.render(market)
                assert(result == Right("Apple Pie: $3 (Flour, Sugar), Banana: $5 (Banana)"))
            }
            test("renders using explicit template name") {
                val ts = templates(
                    template[Ingredient]("ing", "[=name]"),
                ).toOption.get
                val result = ts.render("ing", Ingredient("Flour"))
                assert(result == Right("Flour"))
            }
            test("unknown template name returns TemplateReferenceMissingError") {
                val ts = templates(
                    template[Ingredient]("[=name]"),
                ).toOption.get
                val result = ts.render("unknown", Ingredient("Flour"))
                assert(result.isLeft)
                assert(result.left.toOption.get.isInstanceOf[TemplateReferenceMissingError])
                assert(result.left.toOption.get.asInstanceOf[TemplateReferenceMissingError].templateName == "unknown")
            }
            test("explicit name is used instead of class name") {
                val ts = templates(
                    template[Ingredient]("ing", "[=name]"),
                ).toOption.get
                // class name "Ingredient" is not registered — only "ing" is
                val byClassName = ts.render("Ingredient", Ingredient("Flour"))
                assert(byClassName.isLeft)
                val byExplicitName = ts.render("ing", Ingredient("Flour"))
                assert(byExplicitName == Right("Flour"))
            }
            test("all templates available as refs when rendering by name") {
                val ts = templates(
                    template[Ingredient]("[=name]"),
                    template[Product]("prod", "[=name]: $[=price] ([#list items=ingredients sep=\", \" /])"),
                ).toOption.get
                val product = Product("Apple Pie", 3, List(Ingredient("Flour"), Ingredient("Sugar")))
                val result = ts.render("prod", product)
                assert(result == Right("Apple Pie: $3 (Flour, Sugar)"))
            }
        }

    }

}
