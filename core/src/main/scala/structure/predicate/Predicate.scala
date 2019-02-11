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
    
    
    def validate (madtype : MADType, pred : Predicate) : Boolean = pred match {
        case NoPredicate => true
        case SubPredicate(path, subpred) => madtype == path.on.inner && validate(path.madtype, subpred)
        case AndPredicate(pred1, pred2) => validate(madtype, pred1) && validate(madtype, pred2)
        case IsDefined => madtype.isInstanceOf[MADOption]
    }
    
    import MADNavigable._
    def eval (nav : MADNavigable, pred : Predicate) : Boolean = pred match {
        case NoPredicate => true
        case SubPredicate(path, subpred) => eval(path.navigate(nav), subpred)
        case AndPredicate(pred1, pred2) => eval(nav, pred1) && eval(nav, pred2)
        case IsDefined => nav.asInstanceOf[MADValueOption].exists
    }
}
