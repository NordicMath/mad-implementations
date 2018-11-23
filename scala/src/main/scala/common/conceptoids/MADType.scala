package io.github.nordicmath.mad.conceptoids

abstract class MADType(val name : String)

object MADType {
    case object MADString extends MADType("String")
    case object MADBool extends MADType("Boolean")
    case object MADInt extends MADType("Int")
    case class MADTree(override val name : String, params : (String, MADType)*) extends MADType(name)
    case class MADList(param : MADType) extends MADType("List(" + param.name + ")")
    case class MADOption(param : MADType) extends MADType("Option(" + param.name + ")")    
}
