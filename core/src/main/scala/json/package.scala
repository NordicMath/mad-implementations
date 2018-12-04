package io.github.nordicmath.mad

import org.json4s._

package object json {
    def codec[T : Codec] = implicitly[Codec[T]]
    def encode[T : Codec](x : T) = implicitly[Codec[T]].encode(x)
    def decode[T : Codec](x : JValue) = implicitly[Codec[T]].decode(x)
}
