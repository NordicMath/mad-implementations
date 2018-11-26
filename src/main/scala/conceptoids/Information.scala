package io.github.nordicmath.mad.conceptoids

import scala.reflect.runtime.universe._

import io.github.nordicmath.mad._

import org.json4s._

sealed abstract class Information {
    import Information._
    
    override def toString = this match {
        case NoInformation => f"No information.."
        case NewConceptoid(name) => f"There is a conceptoid with name $name"
        case Apply(path, value) => f"The attribute $path has value $value"
        case OptionAssign(path, false) => f"$path does not make sense"
        case OptionAssign(path, true) => f"$path does makes sense"
        case ListNew(path) => f"There is yet another element in $path"
    }
    
    def toJSON : JValue = this match {
        case NoInformation => JObject("NoInformation" -> JObject())
        case NewConceptoid(pathname) => JObject("NewConceptoid" -> JObject("pathname" -> JString(pathname)))
        case x @ Apply(path, value) => {
            val (tpe : String, v : JValue) = (x.typetag.tpe, value) match {
                case (t, v : Boolean) if t =:= typeOf[Boolean] => ("Boolean", JBool(v))
                case (t, v : String) if t =:= typeOf[String] => ("String", JString(v))
                case (t, v : Int) if t =:= typeOf[Int] => ("Int", JInt(v))
                case _ => throw MADException.InformationJSONUnsupportedType
            }
            
            JObject("Apply" -> JObject("type" -> JString(tpe), "path" -> path.toJSON, "value" -> v))
        }
        case OptionAssign(path, p) => JObject("OptionAssign" -> JObject("path" -> path.toJSON, "possible" -> JBool(p)))
        case ListNew(path) => JObject("ListNew" -> JObject("path" -> path.toJSON))
    }
}

object Information {    
    case object NoInformation extends Information
    
    case class NewConceptoid(name : String) extends Information
    case class Apply[S](path : Path, value : S)(implicit val typetag : TypeTag[S]) extends Information
    case class OptionAssign(path : Path, possible : Boolean) extends Information
    case class ListNew(path : Path) extends Information
    
    def fromJSON (x : JValue) = x match {
        case JObject(List(JField(name, JObject(lst)))) => (name, lst) match {
            case ("NoInformation", List()) => NoInformation
            case ("NewConceptoid", List(JField("pathname", JString(pathname)))) => NewConceptoid(pathname)
            case ("Apply", List(JField("type", JString(tpe)), JField("path", path), JField("value", v))) => (tpe, v) match {
                case ("String", JString(vs)) => Apply[String](Path.fromJSON(path), vs)
                case ("Int", JInt(vi)) => Apply[Int](Path.fromJSON(path), vi.toInt)
                case ("Boolean", JBool(vb)) => Apply[Boolean](Path.fromJSON(path), vb)
            }
            case ("OptionAssign", List(JField("path", path), JField("possible", JBool(p)))) => OptionAssign(Path.fromJSON(path), p)
            case ("ListNew", List(JField("path", path))) => ListNew(Path.fromJSON(path))
        } 
    }
}
