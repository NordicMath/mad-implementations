package io.github.nordicmath.mad.conceptoids

import scala.reflect.runtime.universe._
import io.github.nordicmath.mad._

trait MADNavigable[+T] {
    def madtype : MADType
    
    def set[S : TypeTag](nval : S) : Unit = throw MADException("Not settable!")
    def get : T = throw MADException("Not gettable!")
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
        
    }

    class MADValueTree (name : String, params : Seq[(String, MADType)]) extends MADNavigable[Nothing] {
        
        def madtype = MADTree(name, params : _*)
        
    }

    class MADValueList (param : MADType) extends MADNavigable[Nothing] {
        def madtype = MADList(param)
        
    }

    class MADValueOption (param : MADType) extends MADNavigable[Nothing] {
        def madtype = MADOption(param)
        
    }

}
