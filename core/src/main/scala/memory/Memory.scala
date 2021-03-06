package io.github.nordicmath.mad.memory

import io.github.nordicmath.mad._
import structure._

import java.util.concurrent.TimeUnit
import scala.concurrent._
import scala.concurrent.duration.Duration

trait Memory {
    def madtype : RichMADType
    
    def reset_async() : Future[Unit]
    def reset() : Unit = Await.result(reset_async(), Duration(60, TimeUnit.SECONDS))
    
    def push_async(info : Information) : Future[Unit]
    def push(info : Information) : Unit = Await.result(push_async(info), Duration(60, TimeUnit.SECONDS))
    
    def getObject(path : MADPath) : MADNavigable = path.navigate(getTree)
    def getObjectAs[Nav <: MADNavigable](path : MADPath) : Nav = getObject(path).asInstanceOf[Nav]
    
    def getTree : MADNavigable
    
    def getInformation : Seq[Information]
    def getFailedInformation : Seq[Information]
    
    def close()
}

object Memory {
    def apply(spec : Spec) : Memory = {
        val mem = new InformationBuffer(spec.top)
        spec.info foreach mem.push _
        mem
    }
}
