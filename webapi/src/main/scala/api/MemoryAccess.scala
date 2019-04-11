package io.github.nordicmath.mad.web.api

import io.github.nordicmath.mad._
import memory._
import structure._

trait MemoryAccess {
    def getMemory : Memory 
}

object MemoryAccess {
    def apply(madtype : RichMADType) : MemoryAccess = new MemoryAccess {
        lazy val getMemory = {
            implicit val mem = Memory(madtype)
            new APIInstance().loadInfo()
            mem
        }
    }
}
