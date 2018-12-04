package io.github.nordicmath.mad.json

import org.json4s._


trait Codec[T] {
    def encode(x : T) : JValue
    final def apply(x : T) : JValue = encode(x)
    
    def decode(json : JValue) : Option[T]
    final def unapply(json : JValue) : Option[T] = decode(json)
}

object Codec extends Codecs
