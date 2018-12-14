package io.github.nordicmath.mad.structure

import io.github.nordicmath.mad._
import MADNavigable._
import MADType._

sealed abstract class MADPathInstruction (override val toString : String) 
case class EnterTree(param : String) extends MADPathInstruction(f""""$param"""")
case class EnterList(index : Int) extends MADPathInstruction(index.toString)
case object EnterOption extends MADPathInstruction("EnterOption")

