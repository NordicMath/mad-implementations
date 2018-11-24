package io.github.nordicmath.mad.interpretation

import io.github.nordicmath.mad._

import conceptoids._

trait Interpreter {
    def interpret (path : Path, str : String) : Information
}

object Interpreter {
    def stringInterpreter : Interpreter = new Interpreter {
        def interpret (path : Path, str : String) = Information.Apply[String](path, str)
    }
    
    def intInterpreter : Interpreter = new Interpreter {
        def interpret (path : Path, str : String) = Information.Apply[Int](path, try str.toInt catch {
            case _ : IllegalArgumentException => throw MADException.IntegerInput
        })
    }
    
    def boolInterpreter : Interpreter = new Interpreter {
        def interpret (path : Path, str : String) = Information.Apply[Boolean](path, try str.toBoolean catch {
            case _ : IllegalArgumentException => throw MADException.BooleanInput
        })
    }
    
    def optionInterpreter : Interpreter = new Interpreter {
        def interpret (path : Path, str : String) = Information.OptionAssign(path, try str.toBoolean catch {
            case _ : IllegalArgumentException => throw MADException.BooleanInput
        })
    }
    
    def listInterpreter : Interpreter = new Interpreter {
        def interpret (path : Path, str : String) = if (try str.toBoolean catch {
            case _ : IllegalArgumentException => throw MADException.BooleanInput
        }) Information.ListNew(path) else Information.NoInformation
    }
}
