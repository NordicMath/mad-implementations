package io.github.nordicmath.mad.structure

import io.github.nordicmath.mad._
import MADNavigable._
import MADType._

import util.Try

class MADPath(val on : RichMADType, val instructions : Seq[String]) {
    val madtype = MADPath.validate(on, instructions).getOrElse(throw MADException.MADPathMismatch(this))
    
    def ++(p : MADPath) = MADPath(on, instructions ++ p.instructions)
    
    def /(next : String) : MADPath = MADPath(on, instructions :+ next)
    def /(index : Int) : MADPath = this / index.toString
        
    override def toString = "mad://" + instructions.mkString("/")

    def navigate(nav : MADNavigable) : MADNavigable = if (on == nav.madtype) MADPath.navigate(nav, instructions) else throw MADException.NavigationImpossible(this, nav)
}

object MADPath {
    
    def apply(on : RichMADType, instructions : Seq[String] = Seq()) : MADPath = new MADPath(on, instructions)
    def unapply(p : MADPath) = Some((p.on, p.instructions))
    
    private object Index {
        def unapply(str : String) : Option[Int] = Try(str.toInt).toOption
    }
    
    protected def navigate(nav : MADNavigable, instructions : Seq[String]) : MADNavigable = (nav, instructions) match {
        case (nav, Seq()) => nav
        case (nav : MADValueTree, Seq(param, next @ _*)) => navigate(nav.attr(param), next)
        case (nav : MADValueList, Seq(Index(index), next @ _*)) => navigate(nav.index(index), next)
        case (nav : MADValueOption, Seq("value", next @ _*)) => navigate(nav.internal, next)
        case (nav : MADValueMap, Seq(name, next @ _*)) => navigate(nav.get(name), next)
        case _ => ???
    }
    
    def pathsFrom(nav : MADNavigable) = instructionsFrom(nav).map(MADPath(nav.madtype, _))
    def instructionsFrom(nav : MADNavigable) : Seq[Seq[String]] = (nav match {
        case nav : MADValueTree => for {
            (param, _) <- nav.params
            sub = nav.attr(param)
            subsub <- instructionsFrom(sub)
        } yield param +: subsub
        
        case nav : MADValueList => for {
            (sub, i) <- nav.seq.zipWithIndex
            subsub <- instructionsFrom(sub)
        } yield i.toString +: subsub
        
        case nav : MADValueOption => for {
            sub <- nav.optInternal.toSeq
            subsub <- instructionsFrom(sub)
        } yield "value" +: subsub
        
        case nav : MADValueMap => for {
            name <- nav.names
            sub = nav.get(name)
            subsub <- instructionsFrom(sub)
        } yield name +: subsub
        
        case _ : MADValueSingleton => Seq()
        
        case _ : MADValue[_] => Seq()
    }) :+ Seq()
    
    def validate (on : RichMADType, instructions : Seq[String]) : Option[RichMADType] = (on.inner, instructions) match {
        case (_, Seq()) => Some(on)
        case (MADTree(_, params @ _*), Seq(param, next @ _*)) => params.find(_._1 == param).map(t => validate(t._2, next)).getOrElse(None)
        case (MADList(param), Seq(Index(index), next @ _*)) => if (index < 0) None else validate(param, next)
        case (MADOption(param), Seq("value", next @ _*)) => validate(param, next)
        case (MADMap(param), Seq(_, next @ _*)) => validate(param, next)
        case _ => None
    }
}
