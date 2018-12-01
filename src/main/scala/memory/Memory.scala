package io.github.nordicmath.mad.memory

import io.github.nordicmath.mad._
import conceptoids._

import java.util.concurrent.TimeUnit
import scala.concurrent._
import scala.concurrent.duration.Duration

trait Memory {
    def close()
    def reset_async() : Future[Unit]
    def reset() : Unit = Await.result(reset_async(), Duration(60, TimeUnit.SECONDS))
    
    def push_async(info : Information) : Future[Unit]
    def push(info : Information) : Unit = Await.result(push_async(info), Duration(60, TimeUnit.SECONDS))
    
    def getAttribute(path : Path) : MADNavigable
    def getAttributeAs[Nav <: MADNavigable](path : Path) : Nav = getAttribute(path).asInstanceOf[Nav]
    
    def getObject(name : String) : Conceptoid
    
    def getObjects : Seq[(String, Conceptoid)]
    
    def getInformation : Seq[Information]
    def getFailedInformation : Seq[Information]
}

object Memory {
    def apply() : Memory = new InformationBuffer()
}
