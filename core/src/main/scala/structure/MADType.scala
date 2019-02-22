package io.github.nordicmath.mad.structure

import io.github.nordicmath.mad._
import predicate._

sealed abstract class MADType(val name : String)

object MADType {
    import scala.language.implicitConversions
    implicit def enrich(tp : => MADType) : RichMADType = RichMADType(tp, MADTypeInformation())
    implicit def flatten(rich : RichMADType) : MADType = rich.inner
    
    case object MADString extends MADType("String")
    case object MADBool extends MADType("Boolean")
    case object MADInt extends MADType("Int")
    case class MADTree(override val name : String, params : (String, RichMADType)*) extends MADType(name)
    case class MADList(param : RichMADType) extends MADType("List(" + param.name + ")")
    case class MADOption(param : RichMADType) extends MADType("Option(" + param.name + ")")
    case class MADMap(param : RichMADType) extends MADType("Map(" + param.name + ")")
    case class MADSingleton(sname : String) extends MADType("\"" + sname + "\"")
    case class MADEnum(params : RichMADType*) extends MADType(params.map(_.name).mkString("Either(", ", ", ")"))
    case class MADRef(schema : MADPathSchema, predicate : Option[Predicate] = None) extends MADType("Reference") {
        if(predicate.nonEmpty && !Predicate.validate(schema.pointertype, predicate.get)) throw MADException.InvaldPredicate(schema.pointertype, predicate.get)
        def where (pred : => Predicate) = copy(predicate = Some(predicate.fold(pred)(p => Predicate.AndPredicate(p, pred))))
    }
}
