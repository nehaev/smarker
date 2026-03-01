package io.github.nehaev.smarker

import utest._
import SmarkerModel._
import SmarkerScalaModel._

object SmarkerScalaModelTests extends TestSuite {

    val tests = Tests {

        test("model[T <: Product] — ClassModel from case class") {

            case class Point(x: Int, y: Int) derives CanEqual
            case class Address(street: String, city: String)
            case class User(
                    name: String,
                    age: Int,
                    active: Boolean,
                    tags: List[String],
                    meta: Map[String, String],
                    nickname: Option[String],
                    address: Address,
            )

            test("returns ClassModel") {
                val m = model(Point(1, 2))
                assert(m.isInstanceOf[ClassModel])
            }
            test("getType reflects class name and field types") {
                val m = model(Point(3, 4))
                val pointType = m.getType.asInstanceOf[SmarkerType.Class]
                assert(pointType.name == "Point")
                assert(pointType.fields() == Map("x" -> SmarkerType.Int, "y" -> SmarkerType.Int))
            }
            test("get returns correct primitive fields") {
                val m = model(Point(10, 20))
                assert(m.get("x").get.getUnderlying[Int] == 10)
                assert(m.get("y").get.getUnderlying[Int] == 20)
            }
            test("get missing field returns None") {
                val m = model(Point(1, 2))
                assert(m.get("z") == None)
            }
            test("getUnderlying returns the original case class") {
                val p = Point(7, 8)
                val m = model(p)
                assert(m.getUnderlying[Point] == p)
            }
            test("string field has SmarkerType.String") {
                val m = model(Address("Main St", "Springfield"))
                assert(m.get("street").get.getType == SmarkerType.String)
                assert(m.get("street").get.getUnderlying[String] == "Main St")
            }
            test("bool field has SmarkerType.Bool") {
                case class Flags(enabled: Boolean)
                val m = model(Flags(true))
                assert(m.get("enabled").get.getType == SmarkerType.Bool)
                assert(m.get("enabled").get.getUnderlying[Boolean] == true)
            }
            test("List field has SmarkerType.List") {
                case class Root(items: List[Int])
                val m = model(Root(List(1, 2, 3)))
                assert(m.get("items").get.getType == SmarkerType.List(SmarkerType.Int))
                val lm = m.get("items").get.asInstanceOf[ListModel]
                assert(lm.iterable.toList.map(_.getUnderlying[Int]) == List(1, 2, 3))
            }
            test("Map field has SmarkerType.Map") {
                case class Root(attrs: Map[String, Boolean])
                val m = model(Root(Map("a" -> true, "b" -> false)))
                assert(m.get("attrs").get.getType == SmarkerType.Map(SmarkerType.Bool))
            }
            test("Option Some field is non-empty OptModel") {
                case class Root(score: Option[Int])
                val m = model(Root(Some(42)))
                val opt = m.get("score").get.asInstanceOf[OptModel]
                assert(!opt.isEmpty)
                assert(opt.get.getUnderlying[Int] == 42)
            }
            test("Option None field is empty OptModel") {
                case class Root(score: Option[Int])
                val m = model(Root(None))
                val opt = m.get("score").get.asInstanceOf[OptModel]
                assert(opt.isEmpty)
            }
            test("nested case class field is ClassModel") {
                val m = model(Address("Baker St", "London"))
                val user = model(User("Alice", 30, true, Nil, Map.empty, None, Address("Baker St", "London")))
                val addr = user.get("address").get
                assert(addr.isInstanceOf[ClassModel])
                val addrType = addr.getType.asInstanceOf[SmarkerType.Class]
                assert(addrType.name == "Address")
                assert(addrType.fields() == Map("street" -> SmarkerType.String, "city" -> SmarkerType.String))
                assert(addr.asInstanceOf[ClassModel].get("city").get.getUnderlying[String] == "London")
            }
            test("recursive case class is supported") {
                case class Node(value: Int, next: Option[Node])
                val n2 = Node(2, None)
                val n1 = Node(1, Some(n2))
                val smarkerType = summon[SmarkerTypeOf[Node]].smarkerType
                val nodeType = smarkerType.asInstanceOf[SmarkerType.Class]
                assert(nodeType.name == "Node")
                assert(nodeType.fields()("value") == SmarkerType.Int)
                val nextOptType = nodeType.fields()("next").asInstanceOf[SmarkerType.Opt]
                val innerNodeType = nextOptType.valueType.asInstanceOf[SmarkerType.Class]
                assert(innerNodeType.name == "Node")
                // recursive: inner Node's fields() is accessible without infinite recursion
                assert(innerNodeType.fields()("value") == SmarkerType.Int)
                val m = model(n1)
                val nextOpt = m.get("next").get.asInstanceOf[OptModel]
                assert(!nextOpt.isEmpty)
                val nextNode = nextOpt.get.asInstanceOf[ClassModel]
                assert(nextNode.get("value").get.getUnderlying[Int] == 2)
            }
        }

        test("model[T](Map[String, T]) — MapModel from typed map") {

            test("returns MapModel") {
                val m = model(Map("a" -> "x"))
                assert(m.isInstanceOf[MapModel])
            }
            test("getType reflects value type") {
                val m = model(Map("a" -> 1, "b" -> 2))
                assert(m.getType == SmarkerType.Map(SmarkerType.Int))
            }
            test("keys returns all keys") {
                val m = model(Map("x" -> true, "y" -> false))
                assert(m.keys.toSet == Set("x", "y"))
            }
            test("get returns correct value") {
                val m = model(Map("greeting" -> "hello"))
                assert(m.get("greeting").get.getUnderlying[String] == "hello")
                assert(m.get("greeting").get.getType == SmarkerType.String)
            }
            test("get missing key returns None") {
                val m = model(Map("a" -> 1))
                assert(m.get("b") == None)
            }
            test("Boolean map has SmarkerType.Bool values") {
                val m = model(Map("admin" -> true, "editor" -> false))
                assert(m.getType == SmarkerType.Map(SmarkerType.Bool))
                assert(m.get("admin").get.getUnderlying[Boolean] == true)
            }
            test("empty map has correct type") {
                val m = model(Map.empty[String, Int])
                assert(m.getType == SmarkerType.Map(SmarkerType.Int))
                assert(m.keys.isEmpty)
            }
        }

        test("dynModel(Map[String, Any]) — DynModel from untyped map") {

            test("returns DynModel") {
                val m = dynModel(Map[String, Any]("x" -> 1))
                assert(m.isInstanceOf[DynModel])
            }
            test("getType is SmarkerType.Dyn") {
                val m = dynModel(Map[String, Any]("x" -> 1))
                assert(m.getType == SmarkerType.Dyn)
            }
            test("String field has SmarkerType.String") {
                val m = dynModel(Map[String, Any]("name" -> "Alice"))
                assert(m.get("name").get.getType == SmarkerType.String)
                assert(m.get("name").get.getUnderlying[String] == "Alice")
            }
            test("Int field has SmarkerType.Int") {
                val m = dynModel(Map[String, Any]("count" -> 7))
                assert(m.get("count").get.getType == SmarkerType.Int)
                assert(m.get("count").get.getUnderlying[Int] == 7)
            }
            test("Boolean field has SmarkerType.Bool") {
                val m = dynModel(Map[String, Any]("flag" -> true))
                assert(m.get("flag").get.getType == SmarkerType.Bool)
                assert(m.get("flag").get.getUnderlying[Boolean] == true)
            }
            test("nested Map[String, Any] field is DynModel") {
                val m = dynModel(Map[String, Any]("addr" -> Map[String, Any]("city" -> "Rome")))
                val nested = m.get("addr").get
                assert(nested.isInstanceOf[DynModel])
                assert(nested.asInstanceOf[DynModel].get("city").get.getUnderlying[String] == "Rome")
            }
            test("List field is ListModel with Dyn element type") {
                val m = dynModel(Map[String, Any]("scores" -> List(1, 2, 3)))
                val lm = m.get("scores").get.asInstanceOf[ListModel]
                assert(lm.getType == SmarkerType.List(SmarkerType.Dyn))
                assert(lm.iterable.toList.map(_.getUnderlying[Int]) == List(1, 2, 3))
            }
            test("Option Some field is non-empty OptModel") {
                val m = dynModel(Map[String, Any]("score" -> Some(99)))
                val opt = m.get("score").get.asInstanceOf[OptModel]
                assert(!opt.isEmpty)
                assert(opt.get.getUnderlying[Int] == 99)
            }
            test("Option None field is empty OptModel") {
                val m = dynModel(Map[String, Any]("score" -> Option.empty[Int]))
                val opt = m.get("score").get.asInstanceOf[OptModel]
                assert(opt.isEmpty)
            }
            test("Product field is DynModel with product element names") {
                case class Point(x: Int, y: Int)
                val m = dynModel(Map[String, Any]("origin" -> Point(0, 0)))
                val nested = m.get("origin").get
                assert(nested.isInstanceOf[DynModel])
                assert(nested.asInstanceOf[DynModel].get("x").get.getUnderlying[Int] == 0)
            }
            test("keys returns all keys") {
                val m = dynModel(Map[String, Any]("a" -> 1, "b" -> "two"))
                assert(m.keys.toSet == Set("a", "b"))
            }
            test("get missing key returns None") {
                val m = dynModel(Map[String, Any]("a" -> 1))
                assert(m.get("b") == None)
            }
        }
    }
}
