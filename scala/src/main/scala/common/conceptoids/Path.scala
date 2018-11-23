package io.github.nordicmath.mad.conceptoids

case class Path(cname : String, mpath : MADPath) {
    override def toString = cname + mpath.toString
}
