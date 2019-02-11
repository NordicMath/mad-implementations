package io.github.nordicmath.mad.structure

import predicate._
import Predicate._

package object DSL {
    
    implicit class MADPathStringContextHelper (val sc : StringContext) extends AnyVal {
        def mad(args : Any*) : MADPath = (args, sc.parts) match {
            case (Seq(madtype : MADType), Seq("", str)) if str startsWith "://" => MADPath(madtype, str.split("/").drop(2))
        }
    }
    
    implicit class MADPathPredicateHelper (val path : MADPath) extends AnyVal {
        def exists : Predicate = SubPredicate(path, IsDefined)
    }
    
    import scala.language.implicitConversions
    implicit def toSchema (path : MADPath) : MADPathSchema = MADPathSchema(path)
}
