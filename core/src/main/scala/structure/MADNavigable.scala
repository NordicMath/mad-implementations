package io.github.nordicmath.mad.structure

import io.github.nordicmath.mad._
import json._
import MADType._

import org.json4s._


sealed abstract class MADNavigable (val madtype : RichMADType) {    
    def isset : Boolean
    def unset() : Unit
    
    def toJSON() : JValue
}

object MADNavigable {
    
    sealed abstract class MADValuePrimitive[T](val madtype : MADType)(implicit val codec : Codec[T])
    object MADValuePrimitive {
        implicit object stringPrimitive extends MADValuePrimitive[String](MADString)
        implicit object booleanPrimitive extends MADValuePrimitive[Boolean](MADBool)
        implicit object intPrimitive extends MADValuePrimitive[Int](MADInt)
    }
    
    def apply(x : RichMADType) : MADNavigable = (x : MADType) match {
        case MADString => new MADValue[String](x)
        case MADBool => new MADValue[Boolean](x)
        case MADInt => new MADValue[Int](x)
        case x : MADTree => new MADValueTree(x)
        case x : MADList => new MADValueList(x)
        case x : MADOption => new MADValueOption(x)
        case x : MADMap => new MADValueMap(x)
        case x : MADSingleton => new MADValueSingleton(x)
        case x : MADEnum => new MADValueEnum(x)
    }
    
    class MADValue[T : MADValuePrimitive] (madtype : RichMADType) extends MADNavigable(madtype : RichMADType) {
        
        private var value : Option[T] = None
        def set(nval : T) = value = Some(nval)
        
        def get = value.get
        
        def isset = !value.isEmpty
        def unset() = value = None
        
        private val madvp = implicitly[MADValuePrimitive[T]]
        def toJSON() = if(value.nonEmpty) madvp.codec(value.get) else JNull
    }

    class MADValueTree (madtype : RichMADType) extends MADNavigable(madtype : RichMADType) {
        private[structure] val params = madtype.inner.asInstanceOf[MADTree].params
        
        import collection.mutable.HashMap
        
        private val map : HashMap[String, MADNavigable] = HashMap()
        unset()
        
        def attr(param : String) = map.get(param).get
        
        def isset = map.values.exists(_.isset)
        def unset() = params.foreach {
            case (str, tp) => map.put(str, MADNavigable(tp))
        }
        
        def toJSON() = JObject(params.map {
            case (param, _) => param -> attr(param).toJSON()
        } : _*)
    }

    class MADValueList (madtype : RichMADType) extends MADNavigable(madtype : RichMADType) {
        private val param = madtype.inner.asInstanceOf[MADList].param

        import collection.mutable.Buffer
        
        private val list : Buffer[MADNavigable] = Buffer()
                
        def index(i : Int) = list(i)
        def seq = list.toSeq
        def listNew() : Unit = list += MADNavigable(param)
        
        def isset = false
        def unset() = list.clear()
        
        def toJSON() = JArray(list.toList.map(nav => nav.toJSON))
    }

    class MADValueOption (madtype : RichMADType) extends MADNavigable(madtype : RichMADType) {
        private val param = madtype.inner.asInstanceOf[MADOption].param
        
        private var value : Option[Option[MADNavigable]] = None
                
        def optAssign(possible : Boolean) : Unit = value = Some(if (possible) Some(MADNavigable(param)) else None)
        def internal = value.get.get
        def optInternal = value.flatten
        
        def isset = !value.isEmpty
        def unset() = value = None
        
        def toJSON() = value match {
            case None => JNull
            case Some(None) => JObject("None" -> JNull)
            case Some(Some(nav)) => JObject("Some" -> nav.toJSON())
        }
    }
    
    class MADValueMap (madtype : RichMADType) extends MADNavigable(madtype : RichMADType) {
        private val param = madtype.inner.asInstanceOf[MADMap].param
        
        import collection.mutable.HashMap
        
        private val map : HashMap[String, MADNavigable] = HashMap()
        
        def names : Seq[String] = map.toSeq.map(_._1)
        def contains(name : String) = map.contains(name)
        def get(name : String) : MADNavigable = map(name)
        def put(name : String) : Unit = if (map.put(name, MADNavigable(param)).isDefined) throw MADException.MapNameInUse(name)
        
        def isset = false
        def unset() = map.clear()
        
        def toJSON() = JObject(for {(name, ob) <- map.toList} yield name -> ob.toJSON())
    }

    class MADValueSingleton (madtype : RichMADType) extends MADNavigable(madtype : RichMADType) {
        def isset = true
        def unset() = {}
        
        def toJSON() = JString(madtype.name)
    }
    
    class MADValueEnum (madtype : RichMADType) extends MADNavigable(madtype : RichMADType) {
        private val params = madtype.inner.asInstanceOf[MADEnum].params
        
        private var value : Option[(Int, MADNavigable)] = None 
        
        def get = value.get._2
        def optGet = value.map(_._2)
        def index = value.get._1
        
        def assign(index : Int) = value = Some((index, MADNavigable(params(index))))
        
        def isset = value.isDefined
        def unset() = value = None
        
        def toJSON() = value match {
            case None => JNull
            case Some((index, value)) => JObject("type" -> JString(params(index).name), "value" -> value.toJSON())
        }
        
    }

}
