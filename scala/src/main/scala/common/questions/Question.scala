package io.github.nordicmath.mad.questions

import io.github.nordicmath.mad._
import memory._
import conceptoids._
import interpretation._


case class Question(text : String, path : Path, interpreter : Interpreter)

object QuestionEngine {
    import Interpreter._
    import MADType._
    
    def question (p : Path)(implicit mem : Memory) : Question = mem.getAttribute(p).madtype match {
        case MADString => Question(f"What is $p?", p, stringInterpreter)
        case MADBool => Question(f"Is $p true or false?", p, boolInterpreter)
        case MADInt => Question(f"What number is $p?", p, intInterpreter)
    }
}
