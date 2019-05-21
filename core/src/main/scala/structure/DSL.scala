package io.github.nordicmath.mad.structure

import io.github.nordicmath.mad._

import predicate._
import Predicate._

package object DSL {
    
    implicit class MADPathStringContextHelper (val sc : StringContext) extends AnyVal {
        def mad(args : Any*) : MADPath = (args, sc.parts) match {
            case (Seq(madtype : MADType), Seq("", str)) if str startsWith "://" => MADPath(madtype, str.split("/").drop(2))
            case (Seq(madtype : RichMADType), Seq("", str)) if str startsWith "://" => MADPath(madtype, str.split("/").drop(2))
            case _ => throw MADException.MADPathSyntaxException(args, sc.parts)
        }
    }
    
    implicit class MADPathPredicateHelper (val path : MADPath) extends AnyVal {
        def exists : Predicate = SubPredicate(path, IsDefined)
        def is(value : PredicateValue) : Predicate = SubPredicate(path, IsValue(value))
    }
    
    import scala.language.implicitConversions
    implicit def toSchema (path : MADPath) : MADPathSchema = MADPathSchema(path)
}
