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
    
    case class Destination(override val madtype : RichMADType) extends MADPath(madtype, None){
    }
    
    case class EnterTree(param : String, next : MADPath, override val madtype : RichMADType) extends MADPath(madtype, Some(next)){
    }
    
    case class EnterList(index : Int, next : MADPath, override val madtype : RichMADType) extends MADPath(madtype, Some(next)){
    }
    
    case class EnterOption(next : MADPath, override val madtype : RichMADType) extends MADPath(madtype, Some(next)){
    }
}
