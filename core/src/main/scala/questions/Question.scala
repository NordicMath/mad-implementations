package io.github.nordicmath.mad.questions

import io.github.nordicmath.mad._
import structure._
import interpretation._
import memory._

import Interpreter._
import MADType._

import concurrent.Future
import concurrent.ExecutionContext.Implicits.global



abstract class Question(val text : String) {
    def useSession ()(implicit sess : Session) : Future[Seq[Information]]
}

object Question {
    def apply (text : String, interpreter : Interpreter) = new Question(text) {
        def useSession ()(implicit sess : Session) : Future[Seq[Information]] = for {
            ans <- sess.ask(text)
        } yield interpreter.interpret(ans)
    }
}

class QuestionEngine()(implicit mem : Memory) {
    def questions (p : MADPath) : Seq[Question] = fromMADType(p)
    
    private def fromMADType(p : MADPath) : Seq[Question] = p.madtype.inner match {
        case MADString => Seq(Question(f"What is $p?", stringInterpreter(p)))
        case MADBool => Seq(Question(f"Is $p?", boolInterpreter(p)))
        case MADInt => Seq(Question(f"What number is $p?", intInterpreter(p)))
        case MADOption(_) => Seq(Question(f"Is something like $p possible?", optionInterpreter(p)))
        case MADList(_) => Seq(Question(f"Are there more elements in $p?", listInterpreter(p)))
        case MADMap(_) => Seq(Question(f"Please name another element of $p", mapInterpreter(p)))
        case MADRef(_, _) => Seq(Question(f"What path does $p point to?", pathInterpreter(p)))
        
        case MADSingleton(_) => Seq()
        
        case MADEnum(params @ _*) => Seq(Question(f"""Which of ${params.map(_.name).mkString(" and ")} is $p?""", enumInterpreter(p)))
        
        case MADTree(_, _*) => Seq()
    }
}
