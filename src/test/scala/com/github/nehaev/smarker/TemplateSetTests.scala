package com.github.nehaev.smarker

import utest._

object TemplateSetTests extends TestSuite {

    val tests = Tests {

        case class Ingredient(name: String)
        case class Product(name: String, price: Int, ingredients: List[Ingredient])
        case class Market(products: List[Product])

        test("smarker.template") {
            test("valid source returns Right with correct typeName") {
                val ct = smarker.template[Ingredient]("[=name]")
                assert(ct.isRight)
                assert(ct.toOption.get.typeName == "Ingredient")
            }
            test("invalid source returns Left(SmarkerParseError)") {
                val ct = smarker.template[Ingredient]("[=")
                assert(ct.isLeft)
                assert(ct.left.toOption.get.isInstanceOf[SmarkerParseError])
            }
        }

        test("smarker.templates") {
            test("all valid returns Right(TemplateSet)") {
                val ts = smarker.templates(
                    smarker.template[Ingredient]("[=name]"),
                    smarker.template[Product]("[=name]"),
                )
                assert(ts.isRight)
            }
            test("one parse error returns Left(SmarkerParseError)") {
                val ts = smarker.templates(
                    smarker.template[Ingredient]("[=name]"),
                    smarker.template[Product]("[="),
                )
                assert(ts.isLeft)
                assert(ts.left.toOption.get.isInstanceOf[SmarkerParseError])
            }
        }

        test("TemplateSet.render") {
            test("registered template renders correctly") {
                val ts = smarker.templates(smarker.template[Ingredient]("[=name]")).toOption.get
                val result = ts.render(Ingredient("Flour"))
                assert(result == Right("Flour"))
            }
            test("unregistered type returns TemplateReferenceMissingError") {
                val ts = smarker.templates(smarker.template[Ingredient]("[=name]")).toOption.get
                val result = ts.render(Product("Apple", 3, List()))
                assert(result.isLeft)
                assert(result.left.toOption.get.isInstanceOf[TemplateReferenceMissingError])
            }
            test("all templates in the set are available as templateRefs for body-less directives") {
                val ts = smarker
                    .templates(
                        smarker.template[Ingredient]("[=name]"),
                        smarker.template[Product]("[#list items=ingredients sep=\", \" /]"),
                    )
                    .toOption
                    .get
                val product = Product("Apple Pie", 3, List(Ingredient("Flour"), Ingredient("Sugar")))
                val result = ts.render(product)
                assert(result == Right("Flour, Sugar"))
            }
            test("full example: three-level nested body-less list across templates") {
                val ts = smarker
                    .templates(
                        smarker.template[Ingredient]("[=name]"),
                        smarker.template[Product]("[=name]: $[=price] ([#list items=ingredients sep=\", \" /])"),
                        smarker.template[Market]("[#list items=products sep=\", \" /]"),
                    )
                    .toOption
                    .get
                val market = Market(
                    List(
                        Product("Apple Pie", 3, List(Ingredient("Flour"), Ingredient("Sugar"))),
                        Product("Banana", 5, List(Ingredient("Banana"))),
                    )
                )
                val result = ts.render(market)
                assert(result == Right("Apple Pie: $3 (Flour, Sugar), Banana: $5 (Banana)"))
            }
        }

    }

}
