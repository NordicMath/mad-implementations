package io.github.nordicmath.mad.json

import org.json4s._


trait Encoder[-T] {
    def encode(x : T) : JValue
}

trait Decoder[+T] {
    def decode(json : JValue) : Option[T]
}

trait Codec[T] extends Encoder[T] with Decoder[T]

object Codec extends Codecs
