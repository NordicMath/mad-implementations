package io.github.nordicmath.mad.structure

class RichMADType(madtype : => MADType, val info : MADTypeInformation) {
    lazy val inner = madtype
    override def toString = inner.toString
    override def equals(other : Any) = other match {
        case x : RichMADType => x.inner == inner && x.info == info
        case _ => false
    }
    
    def set[T] (value : PropertyValue[T]) : RichMADType = RichMADType(madtype, info.set(value))
    def get[T] (property : Property[T]) : Option[T] = info.get[T](property)
}

object RichMADType {
    def apply(inner : => MADType, info : MADTypeInformation) = new RichMADType(inner, info)
}

case class MADTypeInformation(info : Seq[PropertyValue[_]]){
    def set[T] (value : PropertyValue[T]) : MADTypeInformation = MADTypeInformation((info filter (_.property != value.property)) :+ value)
    def get[T] (property : Property[T]) : Option[T] = (info find (_.property == property)).map(_.value.asInstanceOf[T])
}

object MADTypeInformation{
    def apply() = new MADTypeInformation(Seq())
}

abstract sealed class Property[T] {
    def := (t : T) = PropertyValue[T](this, t)
}

case object category extends Property[String]
case object priority extends Property[Int]

case class PropertyValue[T](property : Property[T], value : T)
