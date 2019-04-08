package io.github.nordicmath.mad.web.api

import io.github.nordicmath.mad._
import structure._
import memory._
import spec._

import MADNavigable._

import org.json4s._

class APIInstance() {
    implicit val madtype : RichMADType = Conceptoid.Conceptoids
    implicit val memory = Memory(madtype)
    
    def loadInfo() = {
        import scala.io.Source
        val str = Source.fromFile("testing.mad").mkString
        
        import org.json4s.native.JsonMethods._
        val j = parse(str)
        import json._
        val data = decode[Seq[Information]](j).get
        
        memory.reset()
        data foreach memory.push _
        
    }
    
    def get (path : MADPath) : JValue = {
        
        sealed abstract class Element(val json : JValue)
        object Element {
            case class Value[T](name : String, value : Option[T]) extends Element(JObject(
                "type" -> JString("value"),
                "name" -> JString(name), 
                "value" -> value.fold[JValue](JNull)(v => JString(v.toString))
            ))
            case class Link(name : String, path : MADPath) extends Element(JObject(
                "type" -> JString("link"),
                "name" -> JString(name), 
                "path" -> JString(path.toString)
            ))
            case class Ref(name : String, value : Option[MADPath]) extends Element(JObject(
                "type" -> JString("value"),
                "name" -> JString(name), 
                "value" -> value.fold[JValue](JNull)(v => JString(v.toString))
            ))
        }
        
        import Element._
        
        def createElement (name : String, nav : MADNavigable) : Element = {
            nav match {
                case nav : MADValue[_] => Value(name, nav.getOpt)
                case _ : MADValueTree | _ : MADValueList | _ : MADValueMap => Link(name, path / name)
                case nav : MADValueOption => nav.getOpt match {
                    case None => Value[String](name, None)
                    case Some(None) => Value(name, Some("Doesn't exist"))
                    case Some(Some(innernav : MADNavigable)) => createElement(name, innernav)
                }
                case nav : MADValueSingleton => Value(name, Some(nav.madtype.inner.name))
                case nav : MADValueEnum => nav.getOpt.fold[Element](Value(name, None))(createElement(name, _))
                case nav : MADValueRef => Ref(name, nav.getOpt)
            }
        }
        
        val nav = memory.getObject(path)
        val list = nav match {
            case nav : MADValueTree => nav.toSeq.map((createElement _).tupled)
            case nav : MADValueList => for { (innernav, nr) <- nav.seq.zipWithIndex} yield createElement(s"nr. $nr", innernav)
            case nav : MADValueMap => nav.seq.map((createElement _).tupled)
            case _ => Seq(createElement("Value", nav)) 
        }
        
        JArray(list.map(_.json).toList)
    }
}
