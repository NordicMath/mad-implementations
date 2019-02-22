package io.github.nordicmath.mad.questions

import io.github.nordicmath.mad._

import memory._
import structure._

class PriorityEngine(qe : QuestionEngine)(implicit mem : Memory) {
    def generateQuestion() : Question = {
        val questions = generateQuestions()
        
        if(questions.length > 0) questions.apply(0) else throw MADException.NoQuestions
    }
    
    def generatePaths() : Seq[MADPath] = for {
        path <- MADPath.pathsFrom(mem.getTree)
        if !mem.getObject(path).isset
    } yield path
    
    def generateQuestions() : Seq[Question] = for {
        path <- generatePaths()
        question <- qe.questions(path)
    } yield question
}
