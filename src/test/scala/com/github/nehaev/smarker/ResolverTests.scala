package com.github.nehaev.smarker

import utest._
import Ast.AnySpan
import Ast.WithSpan
import Resolver.*
import SmarkerScalaModel.*
import TemplateParser.Impl.templateBody

object ResolverTests extends TestSuite {

    given [T]: Conversion[T, WithSpan[T]] = v => WithSpan(v, AnySpan)

    val tests = Tests {

        val ctx = Context(Map())

        case class Address(street: String, city: String)

        case class User(
                name: String,
                scores: List[Int],
                permissions: Map[String, Boolean],
                age: Option[Int],
                address: Address,
        )

        val classModel = User(
            name = "Alice",
            scores = List(95, 87, 42),
            permissions = Map("admin" -> true, "editor" -> false),
            age = Some(30),
            address = Address("123 Main St", "Anytown"),
        )

        val mapModel = Map(
            "name" -> "Alice",
            "nickname" -> "al",
        )

        test("ServiceMethods") {
            test("resolveScope with map root") {
                val res = resolveScope(model(mapModel), ctx)
                assert(res.isRight)
                val resolvedCtx = res.toOption.get
                assert(resolvedCtx.scope.contains("name"))
                assert(resolvedCtx.scope("name").getType == SmarkerType.String)
                assert(resolvedCtx.scope.contains("nickname"))
                assert(resolvedCtx.scope("nickname").getType == SmarkerType.String)
            }
            test("resolveScope with dyn root") {
                val dynRoot = model(Map[String, Any]("name" -> "Bob", "age" -> 42))
                val res = resolveScope(dynRoot, ctx)
                assert(res.isRight)
                val resolvedCtx = res.toOption.get
                assert(resolvedCtx.scope.contains("name"))
                assert(resolvedCtx.scope("name").getType == SmarkerType.String)
                assert(resolvedCtx.scope.contains("age"))
                assert(resolvedCtx.scope("age").getType == SmarkerType.Int)
            }
            test("resolveScope with class root") {
                val res = resolveScope(model(classModel), ctx)
                assert(res.isRight)
                val resolvedCtx = res.toOption.get
                assert(resolvedCtx.scope.contains("name"))
                assert(resolvedCtx.scope("name").getType == SmarkerType.String)
                assert(resolvedCtx.scope.contains("scores"))
                assert(resolvedCtx.scope("scores").getType == SmarkerType.List(SmarkerType.Int))
                assert(resolvedCtx.scope.contains("permissions"))
                assert(resolvedCtx.scope("permissions").getType == SmarkerType.Map(SmarkerType.Bool))
                assert(resolvedCtx.scope.contains("age"))
                assert(resolvedCtx.scope("age").getType == SmarkerType.Opt(SmarkerType.Int))
                assert(resolvedCtx.scope.contains("address"))
                assert(
                    resolvedCtx.scope("address").getType == SmarkerType.Class(
                        Map(
                            "street" -> SmarkerType.String,
                            "city" -> SmarkerType.String,
                        )
                    )
                )
            }
        }
        test("resolveIdent") {
            val scopeCtx = resolveScope(model(classModel), ctx).toOption.get

            test("finds primitive field") {
                val res = resolveIdent(Ast.Ident("name"), scopeCtx)
                assert(res.isRight)
                assert(res.toOption.get.getType == SmarkerType.String)
                assert(res.toOption.get.getUnderlying[String] == "Alice")
            }
            test("finds list field") {
                val res = resolveIdent(Ast.Ident("scores"), scopeCtx)
                assert(res.isRight)
                assert(res.toOption.get.getType == SmarkerType.List(SmarkerType.Int))
            }
            test("finds map field") {
                val res = resolveIdent(Ast.Ident("permissions"), scopeCtx)
                assert(res.isRight)
                assert(res.toOption.get.getType == SmarkerType.Map(SmarkerType.Bool))
            }
            test("finds option field") {
                val res = resolveIdent(Ast.Ident("age"), scopeCtx)
                assert(res.isRight)
                assert(res.toOption.get.getType == SmarkerType.Opt(SmarkerType.Int))
            }
            test("finds nested class field") {
                val res = resolveIdent(Ast.Ident("address"), scopeCtx)
                assert(res.isRight)
                assert(
                    res.toOption.get.getType == SmarkerType.Class(
                        Map(
                            "street" -> SmarkerType.String,
                            "city" -> SmarkerType.String,
                        )
                    )
                )
            }
            test("undefined variable returns error") {
                val res = resolveIdent(Ast.Ident("nonexistent"), scopeCtx)
                assert(res.isLeft)
                val err = ScopePathMissingError("nonexistent", scopeCtx.scope, scopeCtx, None)
                assert(res.left.toOption.get == err)
            }
            test("works with map-based scope") {
                val mapCtx = resolveScope(model(mapModel), ctx).toOption.get
                val res = resolveIdent(Ast.Ident("nickname"), mapCtx)
                assert(res.isRight)
                assert(res.toOption.get.getUnderlying[String] == "al")
            }
            test("works with dyn-based scope") {
                val dynCtx = resolveScope(model(Map[String, Any]("label" -> "hello")), ctx).toOption.get
                val res = resolveIdent(Ast.Ident("label"), dynCtx)
                assert(res.isRight)
                assert(res.toOption.get.getUnderlying[String] == "hello")
            }
            test("empty scope returns error") {
                val res = resolveIdent(Ast.Ident("name"), ctx)
                assert(res.isLeft)
            }
        }
        test("resolveSelect") {
            val scopeCtx = resolveScope(model(classModel), ctx).toOption.get

            test("map field access") {
                val res = resolveSelect(Ast.Select(Ast.Ident("permissions"), "admin"), scopeCtx)
                assert(res.isRight)
                assert(res.toOption.get.getUnderlying[Boolean] == true)
            }
            test("map field access second key") {
                val res = resolveSelect(Ast.Select(Ast.Ident("permissions"), "editor"), scopeCtx)
                assert(res.isRight)
                assert(res.toOption.get.getUnderlying[Boolean] == false)
            }
            test("undefined field on map returns error") {
                val res = resolveSelect(Ast.Select(Ast.Ident("permissions"), "nonexistent"), scopeCtx)
                assert(res.isLeft)
                val err = ScopePathMissingError("nonexistent", scopeCtx.scope("permissions"), scopeCtx, Some(AnySpan))
                assert(res.left.toOption.get == err)
            }
            test("class field access") {
                val res = resolveSelect(Ast.Select(Ast.Ident("address"), "street"), scopeCtx)
                assert(res.isRight)
                assert(res.toOption.get.getUnderlying[String] == "123 Main St")
            }
            test("class field access second key") {
                val res = resolveSelect(Ast.Select(Ast.Ident("address"), "city"), scopeCtx)
                assert(res.isRight)
                assert(res.toOption.get.getUnderlying[String] == "Anytown")
            }
            test("undefined field on class returns error") {
                val res = resolveSelect(Ast.Select(Ast.Ident("address"), "zipcode"), scopeCtx)
                assert(res.isLeft)
                val err = ScopePathMissingError("zipcode", scopeCtx.scope("address"), scopeCtx, Some(AnySpan))
                assert(res.left.toOption.get == err)
            }
            test("select on primitive fails") {
                val res = resolveSelect(Ast.Select(Ast.Ident("name"), "length"), scopeCtx)
                assert(res.isLeft)
            }
            test("select on list fails") {
                val res = resolveSelect(Ast.Select(Ast.Ident("scores"), "first"), scopeCtx)
                assert(res.isLeft)
            }
            test("undefined root variable") {
                val res = resolveSelect(Ast.Select(Ast.Ident("unknown"), "field"), scopeCtx)
                assert(res.isLeft)
                val err = ScopePathMissingError("unknown", scopeCtx.scope, scopeCtx, None)
                assert(res.left.toOption.get == err)
            }
            test("nested map access") {
                val nestedMap = Map("db" -> Map("host" -> "localhost"))
                val nestedCtx = resolveScope(model(nestedMap), ctx).toOption.get
                val res = resolveSelect(Ast.Select(Ast.Ident("db"), "host"), nestedCtx)
                assert(res.isRight)
                assert(res.toOption.get.getUnderlying[String] == "localhost")
            }
            test("dyn field access") {
                val dynCtx = resolveScope(model(Map[String, Any]("addr" -> Map[String, Any]("city" -> "Springfield"))), ctx).toOption.get
                val res = resolveSelect(Ast.Select(Ast.Ident("addr"), "city"), dynCtx)
                assert(res.isRight)
                assert(res.toOption.get.getUnderlying[String] == "Springfield")
            }
            test("undefined field on dyn returns error") {
                val dynCtx = resolveScope(model(Map[String, Any]("addr" -> Map[String, Any]("city" -> "Springfield"))), ctx).toOption.get
                val res = resolveSelect(Ast.Select(Ast.Ident("addr"), "zip"), dynCtx)
                assert(res.isLeft)
            }
            test("deep chained select") {
                val deepMap = Map("a" -> Map("b" -> Map("c" -> "deep")))
                val deepCtx = resolveScope(model(deepMap), ctx).toOption.get
                val res = resolveSelect(Ast.Select(Ast.Select(Ast.Ident("a"), "b"), "c"), deepCtx)
                assert(res.isRight)
                assert(res.toOption.get.getUnderlying[String] == "deep")
            }
        }
        test("render") {
            test("text mixed with ident interpolations") {
                val tpl = templateBody.parseAll("Hello, [=name]! Welcome to [=address.city].").toOption.get
                val res = render(tpl, model(classModel), ctx)
                assert(res == Right("Hello, Alice! Welcome to Anytown."))
            }
            test("multiline template with several interpolations") {
                val src = "Name: [=name]\nStreet: [=address.street]\nCity: [=address.city]"
                val tpl = templateBody.parseAll(src).toOption.get
                val res = render(tpl, model(classModel), ctx)
                assert(res == Right("Name: Alice\nStreet: 123 Main St\nCity: Anytown"))
            }
            test("literal interpolations mixed with text") {
                val src = "Active: [=true], Count: [=42], Label: [=\"hello\"]"
                val tpl = templateBody.parseAll(src).toOption.get
                val res = render(tpl, model(classModel), ctx)
                assert(res == Right("Active: true, Count: 42, Label: hello"))
            }
            test("adjacent interpolations no text between") {
                val src = "[=name][=address.street][=address.city]"
                val tpl = templateBody.parseAll(src).toOption.get
                val res = render(tpl, model(classModel), ctx)
                assert(res == Right("Alice123 Main StAnytown"))
            }
            test("map root with multiple fields") {
                val src = "[=name] aka [=nickname]"
                val tpl = templateBody.parseAll(src).toOption.get
                val res = render(tpl, model(mapModel), ctx)
                assert(res == Right("Alice aka al"))
            }
            test("nested select through class field") {
                val src = "Lives at [=address.street], [=address.city]"
                val tpl = templateBody.parseAll(src).toOption.get
                val res = render(tpl, model(classModel), ctx)
                assert(res == Right("Lives at 123 Main St, Anytown"))
            }
            test("deep nested select through maps") {
                val deepMap = Map("server" -> Map("db" -> Map("host" -> "localhost")))
                val src = "DB host: [=server.db.host]"
                val tpl = templateBody.parseAll(src).toOption.get
                val res = render(tpl, model(deepMap), ctx)
                assert(res == Right("DB host: localhost"))
            }
            test("dyn root with primitive fields") {
                val dynRoot = model(Map[String, Any]("greeting" -> "Hi", "count" -> 7))
                val src = "[=greeting] x[=count]"
                val tpl = templateBody.parseAll(src).toOption.get
                val res = render(tpl, dynRoot, ctx)
                assert(res == Right("Hi x7"))
            }
            test("nested select through dyn field") {
                val dynRoot = model(Map[String, Any]("loc" -> Map[String, Any]("city" -> "Paris")))
                val src = "City: [=loc.city]"
                val tpl = templateBody.parseAll(src).toOption.get
                val res = render(tpl, dynRoot, ctx)
                assert(res == Right("City: Paris"))
            }
            test("undefined variable in template returns error") {
                val src = "Hello [=unknown]!"
                val tpl = templateBody.parseAll(src).toOption.get
                val res = render(tpl, model(classModel), ctx)
                assert(res.isLeft)
                val err = res.left.toOption.get.asInstanceOf[ScopePathMissingError]
                assert(err.path == "unknown")
                assert(err.span == None)
            }
            test("non-primitive interpolation returns error") {
                val src = "Scores: [=scores]"
                val tpl = templateBody.parseAll(src).toOption.get
                val res = render(tpl, model(classModel), ctx)
                assert(res.isLeft)
                val err = res.left.toOption.get.asInstanceOf[TypeResolutionError]
                assert(err.message == "Unable to render")
                assert(err.expectedType == "primitive")
                assert(err.actualType == SmarkerType.List(SmarkerType.Int))
                assert(err.span == None)
            }
            test("error stops rendering mid-template") {
                val src = "Before [=unknown] After [=name]"
                val tpl = templateBody.parseAll(src).toOption.get
                val res = render(tpl, model(classModel), ctx)
                assert(res.isLeft)
            }
            test("multiline user profile card") {
                val src = """
                    |=================
                    |Name:    [=name]
                    |Address: [=address.street]
                    |         [=address.city]
                    |=================""".trim.stripMargin
                val tpl = templateBody.parseAll(src).toOption.get
                val res = render(tpl, model(classModel), ctx)
                assert(res == Right("""
                    |=================
                    |Name:    Alice
                    |Address: 123 Main St
                    |         Anytown
                    |=================""".trim.stripMargin))
            }
        }
        test("renderIfDefinedDirectiveCall") {
            val scopeCtx = resolveScope(model(classModel), ctx).toOption.get

            test("opt non-empty: body rendered, value bound as _") {
                val src = "[#ifDefined value=age][=_][/#ifDefined]"
                val tpl = templateBody.parseAll(src).toOption.get
                val res = render(tpl, model(classModel), ctx)
                assert(res == Right("30"))
            }
            test("opt non-empty: value bound under explicit alias") {
                val src = "[#ifDefined value=age as=\"a\"]age=[=a][/#ifDefined]"
                val tpl = templateBody.parseAll(src).toOption.get
                val res = render(tpl, model(classModel), ctx)
                assert(res == Right("age=30"))
            }
            test("opt empty: alt string emitted") {
                val src = "[#ifDefined value=age alt=\"unknown\"][=_][/#ifDefined]"
                val data = Map("age" -> Option.empty[Int])
                val tpl = templateBody.parseAll(src).toOption.get
                val res = render(tpl, model(data), ctx)
                assert(res == Right("unknown"))
            }
            test("opt empty: no alt emits nothing") {
                val src = "before[#ifDefined value=age][=_][/#ifDefined]after"
                val data = Map("age" -> Option.empty[Int])
                val tpl = templateBody.parseAll(src).toOption.get
                val res = render(tpl, model(data), ctx)
                assert(res == Right("beforeafter"))
            }
            test("class.field opt empty: alt emitted") {
                case class Profile(bio: Option[String])
                val data = Profile(bio = None)
                val src = "[#ifDefined value=bio alt=\"no bio\"]bio=[=_][/#ifDefined]"
                val tpl = templateBody.parseAll(src).toOption.get
                val res = render(tpl, model(data), ctx)
                assert(res == Right("no bio"))
            }
            test("class.field opt non-empty via class model") {
                case class Profile(bio: Option[String])
                val data = Profile(bio = Some("loves cats"))
                val src = "[#ifDefined value=bio as=\"b\"]bio=[=b][/#ifDefined]"
                val tpl = templateBody.parseAll(src).toOption.get
                val res = render(tpl, model(data), ctx)
                assert(res == Right("bio=loves cats"))
            }
            test("class.optField non-empty: nested fields accessible via default alias _") {
                case class Coords(lat: String, lon: String)
                case class Location(coords: Option[Coords])
                val data = Location(coords = Some(Coords("48.8566", "2.3522")))
                val src = "[#ifDefined value=coords]lat=[=_.lat], lon=[=_.lon][/#ifDefined]"
                val tpl = templateBody.parseAll(src).toOption.get
                val res = render(tpl, model(data), ctx)
                assert(res == Right("lat=48.8566, lon=2.3522"))
            }
            test("map.key present: body receives value") {
                val src = "[#ifDefined value=permissions.admin as=\"v\"]has-admin=[=v][/#ifDefined]"
                val tpl = templateBody.parseAll(src).toOption.get
                val res = render(tpl, model(classModel), ctx)
                assert(res == Right("has-admin=true"))
            }
            test("map.key absent: alt emitted") {
                val src = "[#ifDefined value=permissions.superuser alt=\"no-perm\"][=_][/#ifDefined]"
                val tpl = templateBody.parseAll(src).toOption.get
                val res = render(tpl, model(classModel), ctx)
                assert(res == Right("no-perm"))
            }
            test("dyn opt field non-empty") {
                val dynRoot = model(Map[String, Any]("score" -> Some(99)))
                val src = "[#ifDefined value=score as=\"s\"]score=[=s][/#ifDefined]"
                val tpl = templateBody.parseAll(src).toOption.get
                val res = render(tpl, dynRoot, ctx)
                assert(res == Right("score=99"))
            }
            test("dyn nested map field in body") {
                val dynRoot = model(Map[String, Any]("addr" -> Map[String, Any]("city" -> "Rome")))
                val src = "[#ifDefined value=addr]city=[=_.city][/#ifDefined]"
                val tpl = templateBody.parseAll(src).toOption.get
                val res = render(tpl, dynRoot, ctx)
                assert(res == Right("city=Rome"))
            }
            test("missing value arg returns error") {
                val src = "[#ifDefined][=_][/#ifDefined]"
                val tpl = templateBody.parseAll(src).toOption.get
                val res = render(tpl, model(classModel), ctx)
                assert(res.isLeft)
                val errCtx = res.left.toOption.get.context
                val err = RequiredParamMissingError("ifDefined", "value", errCtx, Some(AnySpan))
                assert(res.left.toOption.get == err)
            }
            test("self-closing returns error") {
                val src = "[#ifDefined value=age /]"
                val tpl = templateBody.parseAll(src).toOption.get
                val res = render(tpl, model(classModel), ctx)
                assert(res.isLeft)
                val errCtx = res.left.toOption.get.context
                val err = RequiredParamMissingError("ifDefined", "body", errCtx, Some(AnySpan))
                assert(res.left.toOption.get == err)
            }
        }

    }

}
