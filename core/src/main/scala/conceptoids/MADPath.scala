package io.github.nordicmath.mad.conceptoids

import io.github.nordicmath.mad._
import MADNavigable._
import MADType._

sealed abstract class MADPath(val madtype : RichMADType, next : Option[MADPath]) {
    if (!validate) throw MADException.MADPathMismatch(this)
    def validate : Boolean
    
    type Nav <: MADNavigable 
    
    final def navigate (nav : MADNavigable) : MADNavigable = nav match {
        case nav if nav.madtype.inner == madtype.inner => inner_navigate(nav.asInstanceOf[Nav])
        case _ => throw MADException.NavigationImpossible(this, nav)
    }
    protected def inner_navigate (nav : Nav) : MADNavigable
    
}

object MADPath {
    
    }
}
