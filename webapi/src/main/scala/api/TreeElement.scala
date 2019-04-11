package io.github.nordicmath.mad.web.api

import io.github.nordicmath.mad._
import structure._

sealed abstract class TreeElement
object TreeElement {
    case class Value[T](name : String, value : Option[T]) extends TreeElement
    case class Link(name : String, path : MADPath) extends TreeElement
    case class Ref(name : String, value : Option[MADPath]) extends TreeElement
}
