package io.github.nordicmath.mad.json

import org.json4s._

object JSON {
    def apply[T : Encoder](x : T) : JValue = implicitly[Encoder[T]].encode(x)
    def unapply[T : Decoder](json : JValue) : Option[T] = implicitly[Decoder[T]].decode(json)
}
