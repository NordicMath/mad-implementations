package io.github.nordicmath.mad

trait IO {
    def read() : String
    def show(str : String)
}

object IO {
    implicit object standardIO extends IO {
        def show(str : String) = println(str)
        def read() = scala.io.StdIn.readLine()
    }
}
