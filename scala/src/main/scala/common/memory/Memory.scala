package io.github.nordicmath.mad.memory

import io.github.nordicmath.mad._
import conceptoids._
import MADType._

trait Memory {
    def close()
    
    def add(info : Information)
    
    def getAttribute(path : Path) : MADNavigable[Any]
    
    def getObject(name : String) : Conceptoid
    
    def getObjects : Seq[(String, Conceptoid)]
}

object Memory {
    def apply() : Memory = new InformationBuffer()
}