package io.github.nordicmath.mad.interpretation

import io.github.nordicmath.mad._
import MADException._
import conceptoids._
import Information._


trait Interpreter {
    def interpret (path : Path, str : String) : Information
}

object Interpreter {
    private def catchIA[T](t : => T, ex : MADException) = try t catch {
        case _ : IllegalArgumentException => throw ex
    }
    
    def stringInterpreter : Interpreter = new Interpreter {
        def interpret (path : Path, str : String) = Apply[String](path, str)
    }
    
    def intInterpreter : Interpreter = new Interpreter {
        def interpret (path : Path, str : String) = Apply[Int](path, catchIA(str.toInt, IntegerInput))
    }
    
    def boolInterpreter : Interpreter = new Interpreter {
        def interpret (path : Path, str : String) = Apply[Boolean](path, catchIA(str.toBoolean, BooleanInput))
    }
    
    def optionInterpreter : Interpreter = new Interpreter {
        def interpret (path : Path, str : String) = OptionAssign(path, catchIA(str.toBoolean, BooleanInput))
    }
    
    def listInterpreter : Interpreter = new Interpreter {
        def interpret (path : Path, str : String) = if (catchIA(str.toBoolean, BooleanInput)) ListNew(path) else NoInformation
    }
}
