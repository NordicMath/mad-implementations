package io.github.nordicmath.mad.json

import org.json4s._

import util.Try

object JSON {
    def apply[T : Encoder](x : T) : JValue = implicitly[Encoder[T]].encode(x)
    def unapply[T : Decoder](json : JValue) : Option[T] = Try(implicitly[Decoder[T]].decode(json)).toOption
}
