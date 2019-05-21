package io.github.nordicmath.mad.qa

import io.github.nordicmath.mad._
import memory._
import spec._

object Main extends App {
    
    implicit val spec : Spec = Conceptoid
    implicit val memory = Memory(spec)
    
    QA().start()
    
    memory.close()
    
}
