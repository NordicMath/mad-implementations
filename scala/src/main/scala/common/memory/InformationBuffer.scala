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
            
            if (!primary.isEmpty) {
                import Information._
                
                primary.dequeue match {
                    case NewConceptoid(p) => if(!mem.contains(p)) mem.put(p, new Conceptoid()) else throw MADException("Path taken! " + p)
                    case x @ Apply(path, value) => {
                        getAttribute(path).set(value)(x.typetag)
                    }
                    case x => throw MADException("InformationBuffer can't handle " + x.toString)
                }
            }

            Thread.sleep(100)
        }
    }
    
    def close() : Unit = running = false
    
    def push(info : Information) : Unit = secondary.enqueue(info)
    def getObject(ob : String) : Conceptoid = mem(ob)
    def getObjects = mem.toSeq
    def getAttribute(path : Path) : MADNavigable[Any] = MADPath.navigate(path.mpath, mem(path.cname).tree)
}
