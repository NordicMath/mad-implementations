package io.github.nordicmath.mad.structure.predicate

import io.github.nordicmath.mad._
import structure._

abstract sealed class Pointer 
object Pointer {
    case class Self(schema : MADPathSchema) extends Pointer
    case class Concrete(mad : MADPath) extends Pointer
    
    import language.implicitConversions
    implicit def concrete(path : MADPath) : Pointer = Concrete(path)
}
