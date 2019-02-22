package io.github.nordicmath.mad.structure

class RichMADType(madtype : => MADType, val info : MADTypeInformation) {
    lazy val inner = madtype
    def priority(p : Int) = RichMADType(inner, info.copy(priority = p))
    override def toString = inner.toString
    override def equals(other : Any) = other match {
        case x : RichMADType => x.inner == inner && x.info == info
        case _ => false
    }
}

object RichMADType {
    def apply(inner : => MADType, info : MADTypeInformation) = new RichMADType(inner, info)
}

case class MADTypeInformation(priority : Int = 5)
