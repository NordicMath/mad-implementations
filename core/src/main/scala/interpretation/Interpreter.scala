package io.github.nordicmath.mad.interpretation

import io.github.nordicmath.mad._
import conceptoids._
import Information._
import MADException._


trait Interpreter {
    def interpret (path : GPath, str : String) : Information
    
    import scala.language.implicitConversions
    implicit def toPath (g : GPath) : Path = g.asInstanceOf[Path]
}

object Interpreter {
    private def catchIA[T](t : => T, ex : MADException) = try t catch {
        case _ : IllegalArgumentException => throw ex
    }
    
    def stringInterpreter : Interpreter = new Interpreter {
        def interpret (path : GPath, str : String) = Apply[String](path, str)
    }
    
    def intInterpreter : Interpreter = new Interpreter {
        def interpret (path : GPath, str : String) = Apply[Int](path, catchIA(str.toInt, IntegerInput))
    }
    
    def boolInterpreter : Interpreter = new Interpreter {
        def interpret (path : GPath, str : String) = Apply[Boolean](path, catchIA(str.toBoolean, BooleanInput))
    }
    
    def optionInterpreter : Interpreter = new Interpreter {
        def interpret (path : GPath, str : String) = OptionAssign(path, catchIA(str.toBoolean, BooleanInput))
    }
    
    def listInterpreter : Interpreter = new Interpreter {
        def interpret (path : GPath, str : String) = if (catchIA(str.toBoolean, BooleanInput)) ListNew(path) else NoInformation
    }
}
