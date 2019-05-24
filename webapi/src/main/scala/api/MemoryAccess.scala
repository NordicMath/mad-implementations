package io.github.nordicmath.mad.web.api

import io.github.nordicmath.mad._
import memory._
import structure._

trait MemoryAccess {
    def getMemory : Memory
}

object MemoryAccess {
    def apply(spec : Spec) : MemoryAccess = new MemoryAccess {
        lazy val getMemory = {
            implicit val mem = Memory(spec)
            //new APIInstance().loadInfo()
            mem
        }
    }
}
