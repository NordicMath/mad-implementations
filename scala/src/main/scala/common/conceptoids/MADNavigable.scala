package io.github.nordicmath.mad.conceptoids

import scala.reflect.runtime.universe._
import io.github.nordicmath.mad._

trait MADNavigable[+T] {
}

object MADNavigable {
    
    import MADType._
    
    def apply(x : MADType) : MADNavigable[Any] = x match {
    }
    
    class MADValue[T : TypeTag] () extends MADNavigable[T] {
    }

    class MADValueTree (name : String, params : Seq[(String, MADType)]) extends MADNavigable[Nothing] {
    }

    class MADValueList (param : MADType) extends MADNavigable[Nothing] {
    }

    class MADValueOption (param : MADType) extends MADNavigable[Nothing] {
    }

}
