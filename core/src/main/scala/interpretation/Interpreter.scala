package io.github.nordicmath.mad.interpretation

import io.github.nordicmath.mad._
import structure._
import Information._
import MADException._


trait Interpreter {
    def interpret (path : MADPath, str : String) : Information
}

object Interpreter {
    private def catchIA[T](t : => T, ex : MADException) = try t catch {
        case _ : IllegalArgumentException => throw ex
    }
    
    def stringInterpreter : Interpreter = new Interpreter {
        def interpret (path : MADPath, str : String) = Apply[String](path, str)
    }
    
    def intInterpreter : Interpreter = new Interpreter {
        def interpret (path : MADPath, str : String) = Apply[Int](path, catchIA(str.toInt, IntegerInput))
    }
    
    def boolInterpreter : Interpreter = new Interpreter {
        def interpret (path : MADPath, str : String) = Apply[Boolean](path, catchIA(str.toBoolean, BooleanInput))
    }
    
    def optionInterpreter : Interpreter = new Interpreter {
        def interpret (path : MADPath, str : String) = OptionAssign(path, catchIA(str.toBoolean, BooleanInput))
    }
    
    def listInterpreter : Interpreter = new Interpreter {
        def interpret (path : MADPath, str : String) = if (catchIA(str.toBoolean, BooleanInput)) ListNew(path) else NoInformation
    }
}
