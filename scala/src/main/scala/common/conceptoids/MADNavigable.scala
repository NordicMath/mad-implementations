package io.github.nordicmath.mad.conceptoids

import scala.reflect.runtime.universe._
import io.github.nordicmath.mad._

trait MADNavigable[+T] {
    def madtype : MADType
    
}

object MADNavigable {
    
    import MADType._
    
    def apply(x : MADType) : MADNavigable[Any] = x match {
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
