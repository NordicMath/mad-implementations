package io.github.nordicmath.mad.conceptoids

case class DBPath(name : String)

case class Path(dbpath : DBPath, mpath : MADPath) {
    override def toString = dbpath.name + mpath.toString
}
