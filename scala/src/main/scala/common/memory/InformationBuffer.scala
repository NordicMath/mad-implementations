package io.github.nordicmath.mad.memory

import io.github.nordicmath.mad._
import conceptoids._
import MADType._

class InformationBuffer() extends Memory {
    
    import collection.mutable.{Queue, HashMap}
    
    // Allows additions to enter secondary while primary queue is locked and being processed
    private val primary : Queue[Information] = Queue()
    private val secondary : Queue[Information] = Queue()
    
    private var locked : Boolean = false
    private var running : Boolean = true
    
    private val mem : HashMap[String, Conceptoid] = HashMap()
    
    private def lock() = locked = true
    private def unlock() = locked = false
    
    import scala.concurrent._
    import ExecutionContext.Implicits.global

    // Buffer loop: 
    private val mainloop = Future {

        while (running) {
            
            if (!locked && !secondary.isEmpty) { 
                primary.enqueue(secondary.dequeue) 
            }
            

            Thread.sleep(100)
        }
    }
    
    def close() : Unit = running = false
    
}
