package io.github.nordicmath.mad.conceptoids

abstract sealed class GPath

case class DBPath(name : String)

case class Path(dbpath : DBPath, mpath : MADPath) extends GPath {
    override def toString = dbpath.name + "." + mpath.toString
}

case object EmptyPath extends GPath {
    override def toString = "root"
}
