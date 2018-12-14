package io.github.nordicmath.mad.structure

import io.github.nordicmath.mad._
import MADNavigable._
import MADType._

sealed abstract class MADPathInstruction (override val toString : String) 
case class EnterTree(param : String) extends MADPathInstruction(f""""$param"""")
case class EnterList(index : Int) extends MADPathInstruction(index.toString)
case object EnterOption extends MADPathInstruction("EnterOption")

class MADPath(val on : RichMADType, val instructions : Seq[MADPathInstruction]) {
    val madtype = MADPath.validate(on, instructions).getOrElse(throw MADException.MADPathMismatch(this))
    
    def ++(p : MADPath) = MADPath(on, instructions ++ p.instructions)
    
    def /(instr : MADPathInstruction) = MADPath(on, instructions :+ instr)
    def /(param : String) = MADPath(on, instructions :+ EnterTree(param))
    def /(index : Int) = MADPath(on, instructions :+ EnterList(index))
    
    override def toString = (f"Root" +: instructions.map(_.toString)).mkString(" / ")

    def navigate(nav : MADNavigable) : MADNavigable = if (on == nav.madtype) MADPath.navigate(nav, instructions) else throw MADException.NavigationImpossible(this, nav)
}

object MADPath {
    
    def apply(on : RichMADType, instructions : Seq[MADPathInstruction] = Seq()) : MADPath = new MADPath(on, instructions)
    def unapply(p : MADPath) = Some((p.on, p.instructions))
    
    protected def navigate(nav : MADNavigable, instructions : Seq[MADPathInstruction]) : MADNavigable = (nav, instructions) match {
    }
    
    def validate (on : RichMADType, instructions : Seq[MADPathInstruction]) : Option[RichMADType] = (on.inner, instructions) match {
    }
}
