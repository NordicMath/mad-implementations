package io.github.nordicmath.mad.conceptoids

abstract class MADPath {
    import MADPath._
    
    override def toString = this match {
        case Destination => ""
        case EnterTree(param, next) => "." + param 
        case EnterList(index, next) => "." + index
    }
    
    def +(other : MADPath) : MADPath = this match {
        case Destination => other
        case EnterTree(param, next) => EnterTree(param, next + other)
        case EnterList(index, next) => EnterList(index, next + other)
    }
}

object MADPath {
    import MADType._
    
    case object Destination extends MADPath
    case class EnterTree(param : String, next : MADPath) extends MADPath
    case class EnterList(index : Int, next : MADPath) extends MADPath
    
}
