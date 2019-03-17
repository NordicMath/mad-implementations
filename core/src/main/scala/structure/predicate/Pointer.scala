package io.github.nordicmath.mad.structure.predicate

import io.github.nordicmath.mad._
import structure._

abstract sealed class Pointer {
    def resolvePath (nav : MADPath) : MADPath
}

object Pointer {
    case class Self(schema : MADPathSchema) extends Pointer {
        def resolvePath (path : MADPath) = schema.fit(path)
    }
    
    case class Concrete(mad : MADPath) extends Pointer {
        def resolvePath (path : MADPath) = mad
    }
    
    import language.implicitConversions
    implicit def concrete(path : MADPath) : Pointer = Concrete(path)
}
