package io.github.nordicmath.mad

import memory._

case class QA()(implicit io : IO, memory : Memory) {
    
    trait Stage {
        def next() : Stage
    }
    
    object Stage {
        private def show(str : String) = io.show(str)
        private def read() = io.read()
    
        private val introOptionsStage = ???
        
        case object Intro extends Stage {
            def next() : Stage = {
                show("Welcome to Zophie!")
                return introOptionsStage
            }
        }
        
    }
    
}
