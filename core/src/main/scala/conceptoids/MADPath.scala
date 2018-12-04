package io.github.nordicmath.mad.conceptoids

import io.github.nordicmath.mad._
import MADNavigable._

sealed abstract class MADPath {
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
    
    def navigate(path : MADPath, nav : MADNavigable) : MADNavigable = (path, nav) match {
        case (Destination, _) => nav
        case (EnterTree(p, next), nav : MADValueTree) => navigate(next, nav.attr(p).get)
        case (EnterList(i, next), nav : MADValueList) => navigate(next, nav.index(i).get)
        case (EnterOption(next), nav : MADValueOption) => navigate(next, nav.getInternalValue)
        case _ => throw MADException.NavigationImpossible(path, nav)
    }
}
