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
            case _ : IllegalArgumentException => throw MADException("Please enter an integer!")
        })
    }
    
    def boolInterpreter : Interpreter = new Interpreter {
        def interpret (path : Path, str : String) = Information.Apply[Boolean](path, try str.toBoolean catch {
            case _ : IllegalArgumentException => throw MADException("Please enter true/false!")
        })
    }
    
    def optionInterpreter : Interpreter = new Interpreter {
        def interpret (path : Path, str : String) = Information.OptionAssign(path, try str.toBoolean catch {
            case _ : IllegalArgumentException => throw MADException("Please enter true/false!")
        })
    }
    
    def listInterpreter : Interpreter = new Interpreter {
        def interpret (path : Path, str : String) = if (try str.toBoolean catch {
            case _ : IllegalArgumentException => throw MADException("Please enter true/false!")
        }) Information.ListNew(path) else Information.NoInformation
    }
}
