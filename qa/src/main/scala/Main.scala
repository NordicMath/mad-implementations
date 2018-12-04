package io.github.nordicmath.mad.qa

import io.github.nordicmath.mad._
import memory._

object Main extends App {
    
    implicit val memory = Memory()
    
    QA().start()
    
    memory.close()
    
}
