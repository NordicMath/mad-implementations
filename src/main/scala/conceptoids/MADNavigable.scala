package io.github.nordicmath.mad.conceptoids

import org.json4s._
import scala.reflect.runtime.universe._

sealed trait MADNavigable {
    def madtype : MADType
    
    def isset : Boolean
    def unset() : Unit
    
    def subpaths : Seq[MADPath] = Seq(MADPath.Destination)
    
    def toJSON() : JValue
}

object MADNavigable {
    
    import MADType._
    
    
    sealed abstract class MADValuePrimitive[T](val madtype : MADType)(implicit val typetag : TypeTag[T]) {
        def conv(x : Any) : T = x.asInstanceOf[T]
        def unapply(x : MADValuePrimitive[Any]) : Option[MADValuePrimitive[T]] = if (typeOf[T] =:= x.typetag.tpe) Some(x.asInstanceOf[MADValuePrimitive[T]]) else None
    }
    
    object MADValuePrimitive {
        implicit object stringPrimitive extends MADValuePrimitive[String](MADString)
        implicit object booleanPrimitive extends MADValuePrimitive[Boolean](MADBool)
        implicit object intPrimitive extends MADValuePrimitive[Int](MADInt)        
    }
    
    def apply(x : MADType) : MADNavigable = x match {
        case MADString => new MADValue[String]()
        case MADBool => new MADValue[Boolean]()
        case MADInt => new MADValue[Int]()
        case MADTree(name, params @ _*) => new MADValueTree(name, params)
        case MADList(param) => new MADValueList(param)
        case MADOption(param) => new MADValueOption(param)
    }
    
    class MADValue[T] ()(implicit val madvp : MADValuePrimitive[T]) extends MADNavigable {
        def madtype = madvp.madtype
        
        private var value : Option[T] = None
        def set(nval : T) = value = Some(nval)
        
        def get = value.get
        
        def isset = !value.isEmpty
        def unset() = value = None
        
        def toJSON() = value match {
            case None => JNull
            case Some(str : String) => JString(str)
            case Some(bool : Boolean) => JBool(bool)
            case Some(num : Int) => JInt(num)
        }
    }

    class MADValueTree (name : String, params : Seq[(String, MADType)]) extends MADNavigable {
        import collection.mutable.HashMap
        
        private val map : HashMap[String, MADNavigable] = HashMap()
        unset()
        
        def madtype = MADTree(name, params : _*)
        
        def attr(param : String) = map.get(param)
        
        def isset = map.values.exists(_.isset)
        def unset() = params.foreach {
            case (str, tp) => map.put(str, MADNavigable(tp))
        }
        
        override def subpaths = for {
            (param, _) <- params
            nav = map(param)
            sub <- nav.subpaths
        } yield MADPath.EnterTree(param, sub)
        
        def toJSON() = JObject(params.map {
            case (param, _) => param -> attr(param).get.toJSON()
        } : _*)
    }

    class MADValueList (param : MADType) extends MADNavigable {
        import collection.mutable.Buffer
        
        private val list : Buffer[MADNavigable] = Buffer()
        
        def madtype = MADList(param)
        
        def index(i : Int) = list.lift(i)
        def listNew() = list += MADNavigable(param)
        
        def isset = false
        def unset() = list.clear()
        
        override def subpaths = {for {
            (nav, i) <- list.toSeq.zipWithIndex
            sub <- nav.subpaths
        } yield MADPath.EnterList(i, sub)} ++ Seq(MADPath.Destination)
        
        def toJSON() = JArray(list.toList.map(nav => nav.toJSON))
    }

    class MADValueOption (param : MADType) extends MADNavigable {
        private var value : Option[Option[MADNavigable]] = None
        
        def madtype = MADOption(param)
        
        def optAssign(possible : Boolean) = value = Some(if (possible) Some(MADNavigable(param)) else None)
        def getInternalValue = value.get.get
        
        def isset = !value.isEmpty
        def unset() = value = None
        
        override def subpaths = value match {
            case None | Some(None) => Seq(MADPath.Destination)
            case Some(Some(nav)) => nav.subpaths.map(MADPath.EnterOption)
        }
        
        def toJSON() = value match {
            case None => JNull
            case Some(None) => JObject("None" -> JNull)
            case Some(Some(nav)) => JObject("Some" -> nav.toJSON())
        }
    }

}
