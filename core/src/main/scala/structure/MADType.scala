package io.github.nordicmath.mad.structure

import predicate._

sealed abstract class MADType(val name : String)

object MADType {
    import scala.language.implicitConversions
    implicit def enrich(tp : MADType) : RichMADType = RichMADType(tp)
    implicit def flatten(rich : RichMADType) = rich.inner
    
    case object MADString extends MADType("String")
    case object MADBool extends MADType("Boolean")
    case object MADInt extends MADType("Int")
    case class MADTree(override val name : String, params : (String, RichMADType)*) extends MADType(name)
    case class MADList(param : RichMADType) extends MADType("List(" + param.name + ")")
    case class MADOption(param : RichMADType) extends MADType("Option(" + param.name + ")")
    case class MADMap(param : RichMADType) extends MADType("Map(" + param.name + ")")
    case class MADSingleton(sname : String) extends MADType("\"" + sname + "\"")
    case class MADEnum(params : RichMADType*) extends MADType(params.map(_.name).mkString("Either(", ", ", ")"))
    case class MADRef(schema : MADPathSchema, filter : Option[Unit => Predicate] = None) extends MADType("Reference") {
        def where (pred : => Predicate) = copy(filter = Some(_ => filter.fold(pred)(p => Predicate.AndPredicate(p(() : Unit), pred))))
        def predicate = filter.map(_())
    }
}
