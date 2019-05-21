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
    def eval (path : MADPath, nav : MADNavigable, pred : Predicate, resolvepath : MADPath) : Boolean = pred match {
        case NoPredicate => true
        case SubPredicate(subpath, subpred) => eval(path ++ subpath, subpath.navigate(nav), subpred, resolvepath)
        case AndPredicate(pred1, pred2) => eval(path, nav, pred1, resolvepath) && eval(path, nav, pred2, resolvepath)
        case IsDefined => nav.asInstanceOf[MADValueOption].exists
        case IsValue(value) => value.resolve(path, nav, resolvepath)
    }
}
