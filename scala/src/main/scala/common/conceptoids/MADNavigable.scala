package io.github.nordicmath.mad.conceptoids

import scala.reflect.runtime.universe._
import io.github.nordicmath.mad._

trait MADNavigable[+T] {
    def madtype : MADType
    
    def set[S : TypeTag](nval : S) : Unit = throw MADException("Not settable!")
    def get : T = throw MADException("Not gettable!")
    def isset : Boolean
    def unset() : Unit
    
}

object MADNavigable {
    
    import MADType._
    
    def apply(x : MADType) : MADNavigable[Any] = x match {
        case MADString => new MADValue[String]()
        case MADBool => new MADValue[Boolean]()
        case MADInt => new MADValue[Int]()
        case MADTree(name, params @ _*) => new MADValueTree(name, params)
        case MADList(param) => new MADValueList(param)
        case MADOption(param) => new MADValueOption(param)
    }
    
    class MADValue[T : TypeTag] () extends MADNavigable[T] {
        def madtype = typeOf[T] match {
            case t if t =:= typeOf[String] => MADString
            case t if t =:= typeOf[Boolean] => MADBool
            case t if t =:= typeOf[Int] => MADInt
            case t => throw MADException("Type not supported by MAD! " + t)
        }
        
        private var value : Option[T] = None
        override def set[S : TypeTag](nval : S) = typeOf[S] match {
            case t if t =:= typeOf[T] => value = Some(nval.asInstanceOf[T])
            case _ => throw MADException("Wrong type!")
        }
        
        override def get = value.get
        
        def isset = !value.isEmpty
        def unset() = value = None
        
    }

    class MADValueTree (name : String, params : Seq[(String, MADType)]) extends MADNavigable[Nothing] {
        import collection.mutable.HashMap
        
        private val map : HashMap[String, MADNavigable[Any]] = HashMap()
        unset()
        
        def madtype = MADTree(name, params : _*)
        
        def isset = map.values.exists(_.isset)
        def unset() = params.foreach {
            case (str, tp) => map.put(str, MADNavigable(tp))
        }
        
    }

    class MADValueList (param : MADType) extends MADNavigable[Nothing] {
        import collection.mutable.Buffer
        
        private val list : Buffer[MADNavigable[Any]] = Buffer()
        
        def madtype = MADList(param)
        
        def isset = true
        def unset() = list.clear()
        
    }

    class MADValueOption (param : MADType) extends MADNavigable[Nothing] {
        private var value : Option[Option[MADNavigable[Any]]] = None
        
        def madtype = MADOption(param)
        
        def isset = !value.isEmpty
        def unset() = value = None
        
    }

}
