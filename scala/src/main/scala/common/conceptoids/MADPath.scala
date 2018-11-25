package io.github.nordicmath.mad.conceptoids

abstract class MADPath {
    import MADPath._
    
    override def toString = this match {
        case Destination => ""
        case EnterTree(param, next) => ".\"" + param + "\"" + next.toString
        case EnterList(index, next) => "." + index + next.toString
        case EnterOption(next) => next.toString
    }
    
    def +(other : MADPath) : MADPath = this match {
        case Destination => other
        case EnterTree(param, next) => EnterTree(param, next + other)
        case EnterList(index, next) => EnterList(index, next + other)
        case EnterOption(next) => EnterOption(next + other)
    }
}

object MADPath {
    
    case object Destination extends MADPath
    case class EnterTree(param : String, next : MADPath) extends MADPath
    case class EnterList(index : Int, next : MADPath) extends MADPath
    case class EnterOption(next : MADPath) extends MADPath
    
    def navigate(path : MADPath, nav : MADNavigable[Any]) : MADNavigable[Any] = path match {
        case Destination => nav
        case EnterTree(p, next) => navigate(next, nav.attr(p).get)
        case EnterList(i, next) => navigate(next, nav.index(i).get)
        case EnterOption(next) => navigate(next, nav.getInternalValue)
    }
}
