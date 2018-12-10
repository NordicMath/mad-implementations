package io.github.nordicmath.mad.structure

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
}
