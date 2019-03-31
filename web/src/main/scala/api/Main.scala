package io.github.nordicmath.mad.web.api

import io.github.nordicmath.mad.structure.DSL._
import org.json4s.native.JsonMethods._

object Main extends App {
    val api = new APIInstance()
    api.loadInfo()
    println(compact(render(api.get(mad"${api.madtype.inner}://"))))
    println(compact(render(api.get(mad"${api.madtype.inner}://Something/description"))))
    println(compact(render(api.get(mad"${api.madtype.inner}://somesome"))))
}
