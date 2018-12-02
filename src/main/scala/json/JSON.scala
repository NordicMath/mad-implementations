package io.github.nordicmath.mad.json

import org.json4s._

class JSON[T : Codec] {
    def apply(x : T) : JValue = implicitly[Encoder[T]].encode(x)
    def unapply(json : JValue) : Option[T] = implicitly[Decoder[T]].decode(json)
}

object JSON {
    def apply[T : Codec](t : T) = implicitly[Encoder[T]].encode(t)
    def apply[T : Codec] = new JSON[T]
}
