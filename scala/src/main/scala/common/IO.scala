package io.github.nordicmath.mad

import scala.reflect.runtime.universe._

trait IO {
    def read() : String
    def show[S : TypeTag](ob : S)
}

object IO {
    object standardIO extends IO {
        def show[S : TypeTag](ob : S) = println(ob.toString)
        def read() = scala.io.StdIn.readLine()
    }
    
    implicit object coloredStdIO extends IO {
        
        def colorOf[S : TypeTag] = typeOf[S] match {
        }
        
        def show[S : TypeTag](ob : S) = standardIO.show[String](colorOf[S] + ob.toString + Console.RESET)
        def read() = standardIO.read()
    }
}
