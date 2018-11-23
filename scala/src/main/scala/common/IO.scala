package io.github.nordicmath.mad

import scala.reflect.runtime.universe._

trait IO {
    def read() : String
    def show[S : TypeTag](ob : S)
}

object IO {
    // Semantic IO objects:
    case class StageOption (override val toString : String)
    
    
    object standardIO extends IO {
        def show[S : TypeTag](ob : S) = println(ob.toString)
        def read() = scala.io.StdIn.readLine()
    }
    
    implicit object coloredStdIO extends IO {
        import conceptoids._
        
        def colorOf[S : TypeTag] = typeOf[S] match {
            // Semantic
            case t if t =:= typeOf[StageOption] => Console.WHITE
            
            // Custom
            case t if t =:= typeOf[Path] => Console.BLUE
            
            case _ => Console.RESET
        }
        
        def show[S : TypeTag](ob : S) = standardIO.show[String](colorOf[S] + ob.toString + Console.RESET)
        def read() = standardIO.read()
    }
}
