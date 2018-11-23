package io.github.nordicmath.mad

trait IO {
    def read() : String
    def show(ob : Any)
}

object IO {
    implicit object standardIO extends IO {
        def show(ob : Any) = println(ob.toString)
        def read() = scala.io.StdIn.readLine()
    }
}
