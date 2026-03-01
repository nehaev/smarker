package com.github.nehaev.smarker

import scala.compiletime.constValue
import scala.compiletime.constValueTuple
import scala.compiletime.erasedValue
import scala.compiletime.error
import scala.compiletime.summonInline
import scala.deriving.Mirror
import SmarkerModel.*

object SmarkerScalaModel {

    // Typeclass to derive SmarkerType from Scala types
    trait SmarkerTypeOf[T] {
        def smarkerType: SmarkerType
    }

    object SmarkerTypeOf {
        def apply[T](using t: SmarkerTypeOf[T]): SmarkerTypeOf[T] = t

        given SmarkerTypeOf[String] with {
            def smarkerType: SmarkerType = SmarkerType.String
        }

        given SmarkerTypeOf[Int] with {
            def smarkerType: SmarkerType = SmarkerType.Int
        }

        given SmarkerTypeOf[Boolean] with {
            def smarkerType: SmarkerType = SmarkerType.Bool
        }

        given [T](using tpe: => SmarkerTypeOf[T]): SmarkerTypeOf[List[T]] with {
            def smarkerType: SmarkerType = SmarkerType.List(tpe.smarkerType)
        }

        given [T](using tpe: => SmarkerTypeOf[T]): SmarkerTypeOf[Map[String, T]] with {
            def smarkerType: SmarkerType = SmarkerType.Map(tpe.smarkerType)
        }

        given [T](using tpe: => SmarkerTypeOf[T]): SmarkerTypeOf[Option[T]] with {
            def smarkerType: SmarkerType = SmarkerType.Opt(tpe.smarkerType)
        }

        // Recursive derivation helpers for SmarkerTypeOf
        inline def summonTypeInstances[T, Elems <: Tuple]: List[SmarkerTypeOf[?]] =
            inline erasedValue[Elems] match {
                case _: (elem *: elems) => deriveOrSummonType[T, elem] :: summonTypeInstances[T, elems]
                case _: EmptyTuple      => Nil
            }

        inline def deriveOrSummonType[T, Elem]: SmarkerTypeOf[Elem] =
            inline erasedValue[Elem] match {
                case _: T => deriveRecType[T, Elem]
                case _    => summonInline[SmarkerTypeOf[Elem]]
            }

        inline def deriveRecType[T, Elem]: SmarkerTypeOf[Elem] =
            inline erasedValue[T] match {
                case _: Elem => error("infinite recursive derivation")
                case _       => SmarkerTypeOf.derived[Elem](using summonInline[Mirror.Of[Elem]])
            }

        class ProductSmarkerType[T](
                className: String,
                fieldNames: List[String],
                fieldTypes: List[SmarkerTypeOf[?]],
        ) extends SmarkerTypeOf[T] {
            private def fieldMap = fieldNames.zip(fieldTypes.map(_.smarkerType)).toMap
            def smarkerType: SmarkerType = SmarkerType.Class(className, () => fieldMap)
        }

        inline given derived[T](using m: Mirror.Of[T]): SmarkerTypeOf[T] = {
            inline m match {
                case p: Mirror.ProductOf[T] =>
                    val x = summonTypeInstances[T, p.MirroredElemTypes]
                    //if (x.exists(_ == null)) error("Unsupported type in product: " + constValue[m.MirroredLabel])
                    new ProductSmarkerType[T](
                        constValue[m.MirroredLabel],
                        getFieldNames[T](using m),
                        x//.filter(_ != null), // we need this due to some compiler bug
                    )
            }
        }
    }

    // Typeclass for converting values to Models
    trait ToModel[T] {
        def apply(x: T): Model
    }

    object ToModel {
        def apply[T](using tm: ToModel[T]): ToModel[T] = tm

        given ToModel[String] with {
            def apply(x: String): Model = new PrimitiveModel {
                def getType: SmarkerType = SmarkerType.String
                def getAsString: String = x
                def getUnderlyingObject: Any = x
            }
        }

        given ToModel[Int] with {
            def apply(x: Int): Model = new PrimitiveModel {
                def getType: SmarkerType = SmarkerType.Int
                def getAsString: String = x.toString
                def getUnderlyingObject: Any = x
            }
        }

        given ToModel[Boolean] with {
            def apply(x: Boolean): Model = new PrimitiveModel {
                def getType: SmarkerType = SmarkerType.Bool
                def getAsString: String = x.toString
                def getUnderlyingObject: Any = x
            }
        }

        // given [T <: Model]: ToModel[T] = identity(_)

        given [T](using tm: => ToModel[T], tpe: => SmarkerTypeOf[T]): ToModel[List[T]] with {
            def apply(x: List[T]): Model = new ListModel {
                def getType: SmarkerType = SmarkerType.List(tpe.smarkerType)
                def iterable: Iterable[Model] = x.map(tm.apply)
                def getUnderlyingObject: Any = x
            }
        }

        given [T](using tm: => ToModel[T], tpe: => SmarkerTypeOf[T]): ToModel[Map[String, T]] with {
            def apply(x: Map[String, T]): Model = new MapModel {
                def getType: SmarkerType = SmarkerType.Map(tpe.smarkerType)
                def keys: Iterable[String] = x.keys
                def get(field: String): Option[Model] = x.get(field).map(tm.apply)
                def getUnderlyingObject: Any = x
            }
        }

        given [T](using tm: => ToModel[T], tpe: => SmarkerTypeOf[T]): ToModel[Option[T]] with {
            def apply(x: Option[T]): Model = new OptModel {
                def getType: SmarkerType = SmarkerType.Opt(tpe.smarkerType)
                def isEmpty: Boolean = x.isEmpty
                def get: Model = x.map(tm.apply).getOrElse(NothingModel)
                def getUnderlyingObject: Any = x
            }
        }

        given ToModel[Map[String, Any]] with {
            def apply(x: Map[String, Any]): Model = dynModel(x)
        }

        // Recursive derivation helpers for ToModel
        inline def summonToModelInstances[T, Elems <: Tuple]: List[ToModel[?]] =
            inline erasedValue[Elems] match {
                case _: (elem *: elems) => deriveOrSummonToModel[T, elem] :: summonToModelInstances[T, elems]
                case _: EmptyTuple      => Nil
            }

        inline def deriveOrSummonToModel[T, Elem]: ToModel[Elem] =
            inline erasedValue[Elem] match {
                case _: T => deriveRecToModel[T, Elem]
                case _    => summonInline[ToModel[Elem]]
            }

        inline def deriveRecToModel[T, Elem]: ToModel[Elem] =
            inline erasedValue[T] match {
                case _: Elem => error("infinite recursive derivation")
                case _       => ToModel.derived[Elem](using summonInline[Mirror.Of[Elem]])
            }

        def convertToModel[T](x: T, fieldNames: List[String], elemInstances: List[ToModel[?]])(using tpe: SmarkerTypeOf[T]): Model = {
            val product = x.asInstanceOf[Product]
            val fieldModels = product.productIterator.toList.lazyZip(elemInstances).map { (value, instance) =>
                instance.asInstanceOf[ToModel[Any]].apply(value)
            }
            val fieldMap = fieldNames.zip(fieldModels).toMap

            new ClassModel {
                def getType: SmarkerType = tpe.smarkerType
                def get(field: String): Option[Model] = fieldMap.get(field)
                def getUnderlyingObject: Any = x
            }
        }

        class ProductToModel[T](
                fieldNames: List[String],
                elemInstances: List[ToModel[?]],
        )(using SmarkerTypeOf[T])
            extends ToModel[T] {
            def apply(x: T): Model = convertToModel(x, fieldNames, elemInstances)
        }

        inline given derived[T](using m: Mirror.Of[T]): ToModel[T] = {
            inline m match {
                case p: Mirror.ProductOf[T] =>
                    new ProductToModel[T](
                        getFieldNames[T](using m),
                        summonToModelInstances[T, p.MirroredElemTypes],
                    )
            }
        }
    }

    // Map model
    def model[T](m: scala.collection.Map[String, T])(using tm: ToModel[T], tpe: SmarkerTypeOf[T]): MapModel = {
        new MapModel {
            def getType: SmarkerType = SmarkerType.Map(tpe.smarkerType)
            def keys: Iterable[String] = m.keys
            def get(field: String): Option[Model] = m.get(field).map(tm.apply)
            def getUnderlyingObject: Any = m
        }
    }

    // Class model - delegates to ToModel
    inline def model[T <: Product](x: T)(using tm: ToModel[T]): ClassModel = {
        tm.apply(x).asInstanceOf[ClassModel]
    }

    // Dyn model - for untyped Map[String, Any]
    def dynModel(fields: Map[String, Any]): DynModel = {
        val modelFields = fields.map((k, v) => k -> anyToModel(v))
        new DynModel {
            def getType: SmarkerType = SmarkerType.Dyn
            def keys: Iterable[String] = modelFields.keys
            def get(field: String): Option[Model] = modelFields.get(field)
            def getUnderlyingObject: Any = fields
        }
    }

    private def anyToModel(v: Any): Model = v match {
        case s: String  => ToModel[String].apply(s)
        case n: Int     => ToModel[Int].apply(n)
        case b: Boolean => ToModel[Boolean].apply(b)
        case l: List[?] =>
            new ListModel {
                def getType: SmarkerType = SmarkerType.List(SmarkerType.Dyn)
                def iterable: Iterable[Model] = l.map(anyToModel)
                def getUnderlyingObject: Any = l
            }
        case o: Option[?] =>
            new OptModel {
                def getType: SmarkerType = SmarkerType.Opt(SmarkerType.Dyn)
                def isEmpty: Boolean = o.isEmpty
                def get: Model = o.map(anyToModel).getOrElse(NothingModel)
                def getUnderlyingObject: Any = o
            }
        case p: Product =>
            dynModel(
                p.productElementNames.zip(p.productIterator).map((k, v) => k -> v).toMap
            )
        case m: Map[_, _] => dynModel(m.asInstanceOf[Map[String, Any]])
        case _            => throw new IllegalArgumentException(s"Unsupported type: ${v.getClass}")
    }

    // Helper methods
    inline def getFieldNames[T](using m: Mirror.Of[T]): List[String] = {
        inline m match {
            case p: Mirror.ProductOf[T] =>
                constValueTuple[p.MirroredElemLabels].toList.asInstanceOf[List[String]]
        }
    }

}
