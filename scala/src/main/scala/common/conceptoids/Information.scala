package io.github.nordicmath.mad.conceptoids

import scala.reflect.runtime.universe._


abstract class Information

object Information {    
    case class NewConceptoid(name : String) extends Information
    case class Apply[S](path : Path, value : S)(implicit val typetag : TypeTag[S]) extends Information
    case class OptionAssign(path : Path, possible : Boolean) extends Information
}
