package io.github.nordicmath.mad.conceptoids

case class Path(cname : String, mpath : MADPath) {
    override def toString = cname + mpath.toString
    
    import org.json4s._

    def toJSON : JValue = JObject(
        "cname" -> JString(cname),
        "mpath" -> mpath.toJSON
    )
}
