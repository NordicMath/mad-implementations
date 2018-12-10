package io.github.nordicmath.mad.qa

import io.github.nordicmath.mad._
import memory._
import conceptoids._

object Main extends App {
    
    implicit val memory = Memory(Conceptoid.Conceptoids)
    
    QA().start()
    
    memory.close()
    
}
