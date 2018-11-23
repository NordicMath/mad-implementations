package io.github.nordicmath.mad.conceptoids



abstract class Information

object Information {    
    case class NewConceptoid(name : String) extends Information
}
