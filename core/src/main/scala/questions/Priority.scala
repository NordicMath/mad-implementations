package io.github.nordicmath.mad.questions

import io.github.nordicmath.mad._

import memory._
import conceptoids._

object PriorityEngine {
    def generateQuestion()(implicit mem : Memory) : Question = {
        val questions = generateQuestions()
        
        if(questions.length > 0) questions.apply(0) else throw MADException.NoQuestions
    }
    
    def generatePaths()(implicit mem : Memory) : Seq[MADPath] = for {
        path <- mem.getTree.subpaths
        if !mem.getObject(path).isset
    } yield path
    
    def generateQuestions()(implicit mem : Memory) : Seq[Question] = for {
        path <- generatePaths()
        question <- QuestionEngine.questions(path)
    } yield question
}
