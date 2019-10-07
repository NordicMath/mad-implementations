package io.github.nordicmath.mad.memory

import io.github.nordicmath.mad._
import structure._
import predicate._
import Information._
import MADNavigable._

import collection.mutable.Queue

import scala.concurrent._
import ExecutionContext.Implicits.global

import util.Try

class InformationBuffer(val madtype : RichMADType) extends Memory {
    
    
    private val informationQueue : Queue[(Information, Promise[Unit])] = Queue()
    
    // All the processed information
    private val finished : Queue[Information] = Queue()
    private val failed : Queue[Information] = Queue()
    
    private var running : Boolean = true
    
    private var resetPromise : Option[Promise[Unit]] = None
    
    private val mem : MADNavigable = MADNavigable(madtype)

    // Buffer loop:
    Future {
        
        while (running) {
            
            if (!informationQueue.isEmpty) {
                
                val (next, p) = informationQueue.dequeue
                
                
                val proc : Try[Unit] = Try(next match {
                    case Apply(path, value) => {
                        getObjectAs[MADValue[Any]](path).set(value)
                    }
                    case OptionAssign(path, b) => {
                        getObjectAs[MADValueOption](path).optAssign(b)
                    }
                    case ListNew(path) => {
                        getObjectAs[MADValueList](path).listNew()
                    }
                    case MapNew(path, name) => {
                        if (name == "") throw MADException.MapNameNotValid
                        getObjectAs[MADValueMap](path).put(name)
                    }
                    case EnumAssign(path, index) => {
                        getObjectAs[MADValueEnum](path).assign(index)
                    }
                    case ReferenceApply(path, value) => {
                        val MADType.MADRef(schema, predicate) = path.madtype.inner.asInstanceOf[MADType.MADRef]
                        
                        if(!schema.check(value)) throw MADException.SchemaFailMADPath
                        val nav = util.Try(getObject(value)).getOrElse(throw MADException.UndefinedMADPath)
                        if(!predicate.fold(true)(Predicate.eval(value, nav, _, path))) throw MADException.PredicateFailMADPath
                        
                        getObjectAs[MADValueRef](path).set_unchecked(value)
                    }
                    case ListStop(path) => {
                        getObjectAs[MADValueList](path).finish()
                    }
                    case MapStop(path) => {
                        getObjectAs[MADValueMap](path).finish()
                    }
                })
                
                p complete proc
                
                (if(proc.isSuccess) finished else failed).enqueue(next)
            }
            
            if (!resetPromise.isEmpty) {
                
                resetPromise.get complete Try({
                    informationQueue.clear()
                    finished.clear()
                    failed.clear()
                    mem.unset()
                })
                
                resetPromise = None
            }
            
            Thread.sleep(InformationBuffer.loopInterval)
        }
    }
    
    def close() : Unit = running = false
    def reset_async() : Future[Unit] = {
        val p = Promise[Unit]()
        resetPromise match {
            case None => resetPromise = Some(p)
            case Some(p1) => p.completeWith(p1.future)
        }
        return p.future
    }
    
    def push_async(info : Information) : Future[Unit] = {
        val p = Promise[Unit]()
        informationQueue.enqueue ((info, p))
        return p.future
    }
    
    def getTree = mem
    def getInformation = finished.toSeq
    def getFailedInformation = failed.toSeq
}

object InformationBuffer {
    val loopInterval = 20
}
