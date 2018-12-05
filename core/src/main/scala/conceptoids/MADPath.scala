package io.github.nordicmath.mad.conceptoids

import io.github.nordicmath.mad._
import MADNavigable._
import MADType._

sealed abstract class MADPath(val madtype : RichMADType, next : Option[MADPath]) {
    type Nav <: MADNavigable 
    
    final def navigate (nav : MADNavigable) : MADNavigable = nav match {
    }
    protected def inner_navigate (nav : Nav) : MADNavigable
    
}

object MADPath {
    
    }
}
