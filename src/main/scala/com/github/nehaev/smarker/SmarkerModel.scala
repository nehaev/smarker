package com.github.nehaev.smarker

object SmarkerModel {

    trait Model derives CanEqual {
        def getType: SmarkerType

        private[smarker] def getUnderlying[T]: T = getUnderlyingObject.asInstanceOf[T]
        protected def getUnderlyingObject: Any

        override def toString(): String = s"Model($getType): $getUnderlyingObject"
    }

    trait PrimitiveModel extends Model {
        def getAsString: String
    }

    trait ListModel extends Model {
        def iterable: Iterable[Model]
        def isEmpty: Boolean = iterable.isEmpty
    }

    trait MapModel extends Model {
        def keys: Iterable[String]
        def get(field: String): Option[Model]
    }

    trait OptModel extends Model {
        def isEmpty: Boolean
        def get: Model
    }

    trait ClassModel extends Model {
        def get(field: String): Option[Model]
    }

    trait DynModel extends Model {
        def keys: Iterable[String]
        def get(field: String): Option[Model]
    }

    case object NothingModel extends Model {
        override def getType: SmarkerType = SmarkerType.Nothing

        override def getUnderlyingObject: Any = ???
    }
}
