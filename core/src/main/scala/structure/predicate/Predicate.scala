package io.github.nordicmath.mad.structure.predicate

import io.github.nordicmath.mad._
import structure._
import MADType._

abstract sealed class Predicate

object Predicate {
    object NoPredicate extends Predicate
    case class SubPredicate (path : MADPath, pred : Predicate) extends Predicate
    case class AndPredicate (pred1 : Predicate, pred2 : Predicate) extends Predicate
    object IsDefined extends Predicate
    case class IsValue (value : PredicateValue) extends Predicate
    
    
    def validate (madtype : MADType, pred : Predicate) : Boolean = pred match {
        case NoPredicate => true
        case SubPredicate(path, subpred) => madtype == path.on.inner && validate(path.madtype, subpred)
        case AndPredicate(pred1, pred2) => validate(madtype, pred1) && validate(madtype, pred2)
        case IsDefined => madtype.isInstanceOf[MADOption]
        case IsValue(value) => value.validate(madtype)
    }
    
    import MADNavigable._
    def eval (nav : MADNavigable, pred : Predicate) : Boolean = pred match {
        case NoPredicate => true
        case SubPredicate(path, subpred) => eval(path.navigate(nav), subpred)
        case AndPredicate(pred1, pred2) => eval(nav, pred1) && eval(nav, pred2)
        case IsDefined => nav.asInstanceOf[MADValueOption].exists
        case IsValue(path, value) => value.resolve(path) == nav
    }
}

abstract sealed trait PredicateValue {
    def validate (madtype : MADType) : Boolean
}

object PredicateValue {
    import language.implicitConversions
    import MADNavigable._
    
    implicit def madFixedValue[T : MADValuePrimitive](value : T) = MADFixedValue[T](value)
    case class MADFixedValue[T](value : T)(implicit t : MADValuePrimitive[T]) extends PredicateValue {
        def validate (madtype : MADType) = t.madtype == madtype
    }
    
    implicit def madRefPointer(p : Pointer) = MADRefPointer(p)
    case class MADRefPointer(p : Pointer) extends PredicateValue {
        def validate (madtype : MADType) = madtype.isInstanceOf[MADRef]
    }
    
    implicit def sequence[T](seq : Seq[T])(implicit conv : T => PredicateValue) = MADSequenceValue(seq.map(conv))
    case class MADSequenceValue[T](seq : Seq[PredicateValue]) extends PredicateValue {
        def validate (madtype : MADType) = if(seq.isEmpty) true else madtype match {
            case MADList(tp) => seq.forall(_.validate(tp))
            case _ => false
        }
        
    }
}
