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
        def interpret (path : Path, str : String) = Information.Apply[Int](path, str.toInt)
    }
    
    def boolInterpreter : Interpreter = new Interpreter {
        def interpret (path : Path, str : String) = Information.Apply[Boolean](path, str.toBoolean)
    }
}
