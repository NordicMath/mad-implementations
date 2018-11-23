package io.github.nordicmath.mad.conceptoids

import org.json4s._
import scala.reflect.runtime.universe._
import io.github.nordicmath.mad._

trait MADNavigable[+T] {
    def madtype : MADType
    
    def attr(str : String) : Option[MADNavigable[Any]] = throw MADException("Not navigable!")
    def index(i : Int) : Option[MADNavigable[Any]] = throw MADException("Not index-navigable!")
    def set[S : TypeTag](nval : S) : Unit = throw MADException("Not settable!")
    def get : T = throw MADException("Not gettable!")
    def isset : Boolean
    def unset() : Unit
    
    
    def toJSON() : JValue
}

object MADNavigable {
    
    import MADType._
    
    def apply(x : MADType) : MADNavigable[Any] = x match {
        case MADString => new MADValue[String]()
        case MADBool => new MADValue[Boolean]()
        case MADInt => new MADValue[Int]()
        case MADTree(name, params @ _*) => new MADValueTree(name, params)
        case MADList(param) => new MADValueList(param)
        case MADOption(param) => new MADValueOption(param)
    }
    
    class MADValue[T : TypeTag] () extends MADNavigable[T] {
        def madtype = typeOf[T] match {
            case t if t =:= typeOf[String] => MADString
            case t if t =:= typeOf[Boolean] => MADBool
            case t if t =:= typeOf[Int] => MADInt
            case t => throw MADException("Type not supported by MAD! " + t)
        }
        
        private var value : Option[T] = None
        override def set[S : TypeTag](nval : S) = typeOf[S] match {
            case t if t =:= typeOf[T] => value = Some(nval.asInstanceOf[T])
            case _ => throw MADException("Wrong type!")
        }
        
        override def get = value.get
        
        def isset = !value.isEmpty
        def unset() = value = None
        
        def toJSON() = value match {
            case None => JNull
            case Some(str : String) => JString(str)
            case Some(bool : Boolean) => JBool(bool)
            case Some(num : Int) => JInt(num)
            case _ => throw MADException("MADValue not JSON-encodable!")
        }
    }

    class MADValueTree (name : String, params : Seq[(String, MADType)]) extends MADNavigable[Nothing] {
        import collection.mutable.HashMap
        
        private val map : HashMap[String, MADNavigable[Any]] = HashMap()
        unset()
        
        def madtype = MADTree(name, params : _*)
        
        override def attr(param : String) = map.get(param)
        
        def isset = map.values.exists(_.isset)
        def unset() = params.foreach {
            case (str, tp) => map.put(str, MADNavigable(tp))
        }
        
        
        def toJSON() = JObject(params.map {
            case (param, _) => param -> attr(param).get.toJSON()
        } : _*)
    }

    class MADValueList (param : MADType) extends MADNavigable[Nothing] {
        import collection.mutable.Buffer
        
        private val list : Buffer[MADNavigable[Any]] = Buffer()
        
        def madtype = MADList(param)
        
        override def index(i : Int) = list.lift(i)
        
        def isset = true
        def unset() = list.clear()
        
        
        def toJSON() = JArray(list.toList.map(nav => nav.toJSON))
    }

    class MADValueOption (param : MADType) extends MADNavigable[Nothing] {
        private var value : Option[Option[MADNavigable[Any]]] = None
        
        def madtype = MADOption(param)
        
        def isset = !value.isEmpty
        def unset() = value = None
        
        
        def toJSON() = value match {
            case None => JNull
            case Some(None) => JObject("None" -> JNull)
            case Some(Some(nav)) => JObject("Some" -> nav.toJSON())
        }
    }

}
