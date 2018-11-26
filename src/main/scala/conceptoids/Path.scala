package io.github.nordicmath.mad.conceptoids

import org.json4s._

case class Path(cname : String, mpath : MADPath) {
    override def toString = cname + mpath.toString
    
    def toJSON : JValue = JObject(
        "cname" -> JString(cname),
        "mpath" -> mpath.toJSON
    )
}

object Path {
    
    def fromJSON (x : JValue) = x match {
        case JObject(List(
            JField("cname", JString(cname)),
            JField("mpath", mpath)
        )) => Path(cname, MADPath.fromJSON(mpath))
    }
}
