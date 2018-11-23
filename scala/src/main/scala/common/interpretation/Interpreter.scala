package io.github.nordicmath.mad.interpretation

import io.github.nordicmath.mad._

import conceptoids._

trait Interpreter {
    def interpret (path : Path, str : String) : Information
}

object Interpreter {
}
