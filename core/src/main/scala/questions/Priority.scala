package io.github.nordicmath.mad.questions

import io.github.nordicmath.mad._

import memory._
import conceptoids._

object PriorityEngine {
    def generateQuestion()(implicit mem : Memory) : Question = {
        val questions = generateQuestions()
        
        if(questions.length > 0) questions.apply(0) else throw MADException.NoQuestions
    }
    
    def generatePaths()(implicit mem : Memory) : Seq[GPath] = (for {
        (cname, conceptoid) <- mem.getObjects
        sub <- conceptoid.tree.subpaths
        path = Path(DBPath(cname), sub)
        if !mem.getAttribute(path).isset
    } yield path) ++ Seq(EmptyPath)
    
    def generateQuestions()(implicit mem : Memory) : Seq[Question] = for {
        path <- generatePaths()
        question <- QuestionEngine.questions(path)
    } yield question
}
