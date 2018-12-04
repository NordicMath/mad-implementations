package io.github.nordicmath.mad.questions

import io.github.nordicmath.mad._
import memory._
import conceptoids._
import interpretation._

import Interpreter._
import MADType._


case class Question(text : String, path : GPath, interpreter : Interpreter)

object QuestionEngine {
    def question (p : GPath)(implicit mem : Memory) : Question = p match {
        case p : Path => fromMadtype(p, mem.getAttribute(p).madtype)
        case EmptyPath => Question(f"What is the path of a new conceptoid?", p, conceptoidPathInterpreter)
    }
    
    private def fromMadtype(p : Path, madtype : MADType) = madtype match {
        case MADString => Question(f"What is $p?", p, stringInterpreter)
        case MADBool => Question(f"Is $p true or false?", p, boolInterpreter)
        case MADInt => Question(f"What number is $p?", p, intInterpreter)
        case MADOption(_) => Question(f"Is something like $p possible?", p, optionInterpreter)
        case MADList(_) => Question(f"Are there more elements in $p?", p, listInterpreter)
        
        case mt => throw MADException.QuestionUnsupportedType(p, mt)
    }
}
