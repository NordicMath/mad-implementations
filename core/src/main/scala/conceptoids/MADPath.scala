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
        def validate = true
        
    }
    
    case class EnterTree(param : String, next : MADPath, override val madtype : RichMADType) extends MADPath(madtype, Some(next)){
        def validate = madtype.inner match {
            case tp : MADTree => tp.params.map(_._1).contains(param)
            case _ => false
        }
        
    }
    
    case class EnterList(index : Int, next : MADPath, override val madtype : RichMADType) extends MADPath(madtype, Some(next)){
        def validate = madtype.inner.isInstanceOf[MADList]
        
    }
    
    case class EnterOption(next : MADPath, override val madtype : RichMADType) extends MADPath(madtype, Some(next)){
        def validate = madtype.inner.isInstanceOf[MADOption]
        
    }
}
