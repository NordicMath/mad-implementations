package io.github.nordicmath.mad.conceptoids

import org.json4s._

case class Path(cname : String, mpath : MADPath) {
    override def toString = cname + mpath.toString
}
