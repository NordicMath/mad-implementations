package io.github.nordicmath.mad.qa

import io.github.nordicmath.mad._
import questions._

import scala.reflect.runtime.universe._

import concurrent.Future
import concurrent.ExecutionContext.Implicits.global


trait IO extends Session {
    def read() : String
    def show[S : TypeTag](ob : S)
    def ask(str : String) : Future[String] = Future {
        show(str)
        read()
    }
}

object IO {
    // Semantic IO objects:
    case class StageOption (override val toString : String)
    
    
    object standardIO extends IO {
        def show[S : TypeTag](ob : S) = println(ob.toString)
        def read() = scala.io.StdIn.readLine()
    }
    
    implicit object improvedIO extends IO {
        import structure._
        
        import org.json4s._
        import org.json4s.native.JsonMethods._
        
        def colorOf[S : TypeTag] = typeOf[S] match {
            // Semantic
            case t if t =:= typeOf[StageOption] => Console.WHITE
            
            // Custom
            case t if t =:= typeOf[MADPath] => Console.BLUE
            case t if t =:= typeOf[Information] => Console.BLUE
            case t if t =:= typeOf[JValue] => Console.RED
            
            case _ => Console.RESET
        }
        
        def show[S : TypeTag](ob : S) = {
            val txt : String = typeOf[S] match {
                case t if t <:< typeOf[JValue] => pretty(render(ob.asInstanceOf[JValue]))
                
                case _ => ob.toString
            }
            
            standardIO.show[String](colorOf[S] + txt + Console.RESET)
        }
        def read() = standardIO.read()
    }
}
