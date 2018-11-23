package io.github.nordicmath.mad.conceptoids

import io.github.nordicmath.mad._

trait MADNavigable[+T] {
}

object MADNavigable {
    
    import MADType._
    
    def apply(x : MADType) : MADNavigable[Any] = x match {
    }
    
}
