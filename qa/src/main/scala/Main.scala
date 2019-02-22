package io.github.nordicmath.mad.qa

import io.github.nordicmath.mad._
import memory._
import spec._
import structure._

object Main extends App {
    
    implicit val madtype : RichMADType = Conceptoid.Conceptoids
    implicit val memory = Memory(madtype)
    
    QA().start()
    
    memory.close()
    
}
