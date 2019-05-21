package io.github.nordicmath.mad.structure.predicate

import io.github.nordicmath.mad._
import structure._
import MADType._

abstract sealed trait PredicateValue {
    def validate (madtype : MADType) : Boolean
    def resolve (path : MADPath, nav : MADNavigable, resolvepath : MADPath) : Boolean
}

object PredicateValue {
    import language.implicitConversions
    import MADNavigable._
    
    implicit def madFixedValue[T : MADValuePrimitive](value : T) = MADFixedValue[T](value)
    case class MADFixedValue[T](value : T)(implicit t : MADValuePrimitive[T]) extends PredicateValue {
        def validate (madtype : MADType) = t.madtype == madtype
        
        def resolve (path : MADPath, nav : MADNavigable, resolvepath : MADPath) : Boolean = {
            nav.asInstanceOf[MADValue[T]].getOpt.fold(false)(_ == value)
        }
    }
    
    implicit def madRefPointer(p : Pointer) = MADRefPointer(p)
    case class MADRefPointer(p : Pointer) extends PredicateValue {
        def validate (madtype : MADType) = madtype.isInstanceOf[MADRef]
        
        def resolve (path : MADPath, nav : MADNavigable, resolvepath : MADPath) : Boolean = {
            nav.asInstanceOf[MADValueRef].getOpt.fold(false)(_ == p.resolvePath(resolvepath))
        }
    }
    
    implicit def sequence[T](seq : Seq[T])(implicit conv : T => PredicateValue) = MADSequenceValue(seq.map(conv))
    case class MADSequenceValue[T](seq : Seq[PredicateValue]) extends PredicateValue {
        def validate (madtype : MADType) = if(seq.isEmpty) true else madtype match {
            case MADList(tp) => seq.forall(_.validate(tp))
            case _ => false
        }
        
        def resolve (path : MADPath, nav : MADNavigable, resolvepath : MADPath) : Boolean = {
            val navlist = nav.asInstanceOf[MADValueList].navlist
            navlist.length == seq.length && (navlist zip seq).forall {
                case (subnav, predval) => predval.resolve(path, subnav, resolvepath)
            }
        }
    }
}
