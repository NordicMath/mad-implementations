package io.github.nordicmath.mad.questions

import concurrent.Future

trait Session {
    def ask(str : String) : Future[String]
}
