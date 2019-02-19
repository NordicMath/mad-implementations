package io.github.nordicmath.mad.structure

class RichMADType(madtype : => MADType, val info : MADTypeInformation) {
    def inner = madtype
    def priority(p : Int) = RichMADType(inner, info.copy(priority = p))
}

object RichMADType {
    def apply(inner : => MADType, info : MADTypeInformation) = new RichMADType(inner, info)
}

case class MADTypeInformation(priority : Int = 5)
