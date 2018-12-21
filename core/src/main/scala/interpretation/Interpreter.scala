package io.github.nordicmath.mad.interpretation

import io.github.nordicmath.mad._
import structure._
import Information._
import MADException._


trait Interpreter {
    def interpret (str : String) : Information
}

object Interpreter {
    
    private def parseBool(str : String) = str match {
        case "true" | "yes" => true
        case "false" | "no" => false
        case _ => BooleanInput
    }
    
    private def parseInt(str : String) = try str.toInt catch {
        case _ : IllegalArgumentException => throw IntegerInput
    }
    
    
    def stringInterpreter(path : MADPath) : Interpreter = new Interpreter {
        def interpret (str : String) = Apply[String](path, str)
    }
    
    def intInterpreter(path : MADPath) : Interpreter = new Interpreter {
        def interpret (str : String) = Apply[Int](path, parseInt(str))
    }
    
    def boolInterpreter(path : MADPath) : Interpreter = new Interpreter {
        def interpret (str : String) = Apply[Boolean](path, parseBool(str))
    }
    
    def optionInterpreter(path : MADPath) : Interpreter = new Interpreter {
        def interpret (str : String) = OptionAssign(path, parseBool(str))
    }
    
    def listInterpreter(path : MADPath) : Interpreter = new Interpreter {
        def interpret (str : String) = if parseBool(str) ListNew(path) else NoInformation
    }
    
    def mapInterpreter(path : MADPath) : Interpreter = new Interpreter {
        def interpret (str : String) = MapNew(path, str)
    }
}
