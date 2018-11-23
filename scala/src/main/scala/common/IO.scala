package io.github.nordicmath.mad

import scala.reflect.runtime.universe._

trait IO {
    def read() : String
    def show[S : TypeTag](ob : S)
}

object IO {
    implicit object standardIO extends IO {
        def show[S : TypeTag](ob : S) = println(ob.toString)
        def read() = scala.io.StdIn.readLine()
    }
}
