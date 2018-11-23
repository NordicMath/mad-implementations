package io.github.nordicmath.mad.conceptoids

import scala.reflect.runtime.universe._


abstract class Information

object Information {    
    case class NewConceptoid(name : String) extends Information
    case class Apply[S : TypeTag](path : Path, value : S) extends Information
}
