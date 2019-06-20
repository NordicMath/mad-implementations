package io.github.nordicmath.mad.structure

import io.github.nordicmath.mad._

import predicate._
import Predicate._

package object DSL {
    
    implicit class MADPathStringContextHelper (val sc : StringContext) extends AnyVal {
        def mad(args : Any*) : MADPath = (args, sc.parts) match {
            case (Seq(madtype, otherargs @ _*), Seq("", str @ _*)) => (madtype, new StringContext(str : _*).s(otherargs : _*)) match {
                case (madtype : MADType, str : String) if str startsWith "://" => MADPath(madtype, str.split("/").drop(2))
                case (madtype : RichMADType, str : String) if str startsWith "://" => MADPath(madtype, str.split("/").drop(2))
                case _ => throw MADException.MADPathSyntaxException(args, sc.parts)
            }
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
