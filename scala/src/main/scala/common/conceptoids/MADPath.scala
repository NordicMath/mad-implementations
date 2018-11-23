package io.github.nordicmath.mad.conceptoids

abstract class MADPath {
}

object MADPath {
    import MADType._
    
    case object Destination extends MADPath
    case class EnterTree(param : String, next : MADPath) extends MADPath
    case class EnterList(index : Int, next : MADPath) extends MADPath
    
}
