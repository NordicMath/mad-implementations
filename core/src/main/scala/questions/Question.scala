package io.github.nordicmath.mad.questions

import io.github.nordicmath.mad._
import structure._
import interpretation._

import Interpreter._
import MADType._


case class Question(text : String, path : MADPath, interpreter : Interpreter)

object QuestionEngine {
    def questions (p : MADPath) : Seq[Question] = fromMADType(p, p.madtype)
    
    private def fromMADType(p : MADPath, madtype : RichMADType) : Seq[Question] = madtype.inner match {
        case MADString => Seq(Question(f"What is $p?", p, stringInterpreter))
        case MADBool => Seq(Question(f"Is $p true or false?", p, boolInterpreter))
        case MADInt => Seq(Question(f"What number is $p?", p, intInterpreter))
        case MADOption(_) => Seq(Question(f"Is something like $p possible?", p, optionInterpreter))
        case MADList(_) => Seq(Question(f"Are there more elements in $p?", p, listInterpreter))
        
        case MADTree(_, _*) => Seq()
        case mt => throw MADException.QuestionUnsupportedType(p, mt)
    }
}
