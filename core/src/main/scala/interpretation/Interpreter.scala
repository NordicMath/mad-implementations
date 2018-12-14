package io.github.nordicmath.mad.interpretation

import io.github.nordicmath.mad._
import structure._
import Information._
import MADException._


trait Interpreter {
    def interpret (str : String) : Information
}

object Interpreter {
    private def catchIA[T](t : => T, ex : MADException) = try t catch {
        case _ : IllegalArgumentException => throw ex
    }
    
    def stringInterpreter(path : MADPath) : Interpreter = new Interpreter {
        def interpret (str : String) = Apply[String](path, str)
    }
    
    def intInterpreter(path : MADPath) : Interpreter = new Interpreter {
        def interpret (str : String) = Apply[Int](path, catchIA(str.toInt, IntegerInput))
    }
    
    def boolInterpreter(path : MADPath) : Interpreter = new Interpreter {
        def interpret (str : String) = Apply[Boolean](path, catchIA(str.toBoolean, BooleanInput))
    }
    
    def optionInterpreter(path : MADPath) : Interpreter = new Interpreter {
        def interpret (str : String) = OptionAssign(path, catchIA(str.toBoolean, BooleanInput))
    }
    
    def listInterpreter(path : MADPath) : Interpreter = new Interpreter {
        def interpret (str : String) = if (catchIA(str.toBoolean, BooleanInput)) ListNew(path) else NoInformation
    }
}
