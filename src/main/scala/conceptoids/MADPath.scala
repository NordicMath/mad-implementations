package io.github.nordicmath.mad.conceptoids

import io.github.nordicmath.mad._

import org.json4s._

sealed abstract class MADPath {
    import MADPath._
    
    override def toString = this match {
        case Destination => ""
        case EnterTree(param, next) => ".\"" + param + "\"" + next.toString
        case EnterList(index, next) => "." + index + next.toString
        case EnterOption(next) => next.toString
    }
    
    def toJSON : JValue = this match {
        case Destination => JString("Destination")
        case EnterTree(param, next) => JObject("param" -> JString(param), "next" -> next.toJSON)
        case EnterList(index, next) => JObject("index" -> JInt(index), "next" -> next.toJSON)
        case EnterOption(next) => JObject("enter" -> JObject(), "next" -> next.toJSON)
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
    
    import MADNavigable._
    
    def navigate(path : MADPath, nav : MADNavigable[Any]) : MADNavigable[Any] = (path, nav) match {
        case (Destination, _) => nav
        case (EnterTree(p, next), nav : MADValueTree) => navigate(next, nav.attr(p).get)
        case (EnterList(i, next), nav : MADValueList) => navigate(next, nav.index(i).get)
        case (EnterOption(next), nav : MADValueOption) => navigate(next, nav.getInternalValue)
        case _ => throw MADException.NavigationImpossible(path, nav)
    }
    
    def fromJSON(x : JValue) : MADPath = x match {
        case JString("Destination") => Destination
        case JObject(List(JField("param", JString(param)), JField("next", next))) => EnterTree(param, fromJSON(next))
        case JObject(List(JField("index", JInt(index)), JField("next", next))) => EnterList(index.toInt, fromJSON(next))
        case JObject(List(JField("enter", JObject(List())), JField("next", next))) => EnterOption(fromJSON(next))
    }
}
