package io.github.nordicmath.mad.memory

import io.github.nordicmath.mad._
import conceptoids._
import Information._
import MADNavigable._

import collection.mutable.{Queue, HashMap}

import scala.concurrent._
import ExecutionContext.Implicits.global

import util.Try

class InformationBuffer() extends Memory {
    
    
    private val informationQueue : Queue[(Information, Promise[Unit])] = Queue()
    
    // All the processed information
    private val finished : Queue[Information] = Queue()
    private val failed : Queue[Information] = Queue()
    
    private var running : Boolean = true
    
    private val mem : HashMap[String, Conceptoid] = HashMap()

    // Buffer loop: 
    Future {

        while (running) {
            
            if (!informationQueue.isEmpty) {
                
                val (next, p) = informationQueue.dequeue
                
                
                val proc : Try[Unit] = Try(next match {
                    case NoInformation => {}
                    
                    case NewConceptoid(p) => {
                        if(!mem.contains(p)) 
                            mem.put(p, new Conceptoid()) 
                        else throw MADException.ConceptoidNameTaken(p)
                    }
                    case x @ Apply(path, value) => {
                        getAttributeAs[MADValue[Any]](path).set(value)(x.typetag)
                    }
                    case OptionAssign(path, b) => {
                        getAttributeAs[MADValueOption](path).optAssign(b)
                    }
                    case ListNew(path) => {
                        getAttributeAs[MADValueList](path).listNew()
                    }
                    case x => throw MADException.InformationUnhandled(x, "InformationBuffer")
                })
                
                p complete proc
                
                (if(proc.isSuccess) finished else failed).enqueue(next)
            }

            Thread.sleep(InformationBuffer.loopInterval)
        }
    }
    
    def close() : Unit = running = false
    
    def push_async(info : Information) : Future[Unit] = {
        val p = Promise[Unit]()
        informationQueue.enqueue ((info, p))
        return p.future
    }
    
    def getObject(ob : String) : Conceptoid = mem(ob)
    def getObjects = mem.toSeq
    def getAttribute(path : Path) : MADNavigable[Any] = MADPath.navigate(path.mpath, mem(path.cname).tree)
    def getInformation = finished.toSeq
}

object InformationBuffer {
    val loopInterval = 20
}
