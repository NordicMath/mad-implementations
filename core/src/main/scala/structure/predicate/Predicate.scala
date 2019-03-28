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
    case class IsValue (path : MADPath, value : PredicateValue) extends Predicate
    
    
    def validate (madtype : MADType, pred : Predicate) : Boolean = pred match {
        case NoPredicate => true
        case SubPredicate(path, subpred) => madtype == path.on.inner && validate(path.madtype, subpred)
        case AndPredicate(pred1, pred2) => validate(madtype, pred1) && validate(madtype, pred2)
        case IsDefined => madtype.isInstanceOf[MADOption]
        case IsValue(_, value) => value.validate(madtype)
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

abstract sealed class PredicateValue { 
    import PredicateValue._
    
    def validate (madtype : MADType) : Boolean 
    def information (madtype : MADType, path : MADPath) : Seq[Information]
    final def information (path : MADPath) : Seq[Information] = information(path.madtype, path)
    final def resolve (path : MADPath) : MADNavigable = createNavigable(path.madtype, information(path))
}

object PredicateValue {
    import language.implicitConversions
    
    import MADNavigable._
    import Information._
    import DSL._
    
    // This should be moved somewhere more general. Perhaps it is slow?
    def createNavigable(madtype : RichMADType, info : Seq[Information]) : MADNavigable = {
        import memory._
        val mem = Memory(madtype)
        info foreach {info => mem.push(info)}
        mem.close()
        mem.getTree
    }
    
    implicit def madFixedValue[T : MADValuePrimitive](value : T) = MADFixedValue[T](value)
    case class MADFixedValue[T](value : T)(implicit t : MADValuePrimitive[T]) extends PredicateValue {
        def validate (madtype : MADType) = t.madtype == madtype
        //def resolve (path : MADPath) = createNavigable(t.madtype, Apply(mad"${t.madtype}://", value))
        def information (mad : MADType, path : MADPath) = Seq(Apply(mad"$mad://", value))
    }
    
    implicit def madRefPointer(p : Pointer) = MADRefPointer(p)
    case class MADRefPointer(p : Pointer) extends PredicateValue {
        def validate (madtype : MADType) = madtype.isInstanceOf[MADRef]
        //def resolve (path : MADPath) = createNavigable(nav.madtype, ReferenceApply(mad"${path.madtype}://", p.resolvePath(path)))
        def information (mad : MADType, path : MADPath) = Seq(ReferenceApply(mad"${path.madtype}://", p.resolvePath(path)))
    }
    
    implicit def sequence[T](seq : Seq[T])(implicit conv : T => PredicateValue) = MADSequenceValue(seq.map(conv))
    case class MADSequenceValue[T](seq : Seq[PredicateValue]) extends PredicateValue {
        def validate (madtype : MADType) = if(seq.isEmpty) true else madtype match { 
            case MADList(tp) => seq.forall(_.validate(tp))
            case _ => false
        }
        
        //def resolve (madtype : MADType)
        def information (mad : MADType, path : MADPath) = seq.flatMap(ListNew(mad"mad://") +: _.information(path))
    }
}
