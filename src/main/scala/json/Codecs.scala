package io.github.nordicmath.mad.json

import org.json4s._

import util.Try

trait Codecs {
    case class SCodec[S <: JValue, T](encoder : T => S, decoder : S => Option[T]) extends Codec[T] {
        def embed (j : JValue) : Option[S] = Try(j.asInstanceOf[S]).toOption
        
        def encode(t : T) = encoder(t)
        def decode(j : JValue) = for {s <- embed(j); t <- decoder(s)} yield t
    }
    
    case class TCodec[T, S : Codec](to : T => S, from : S => T) extends Codec[T] {
        def encode(t : T) = JSON[S](to(t))
        def decode(j : JValue) = JSON[S].unapply(j).map(from)
    }
        
    implicit object StringCodec extends SCodec[JString, String](JString.apply, JString.unapply)
    implicit object BooleanCodec extends SCodec[JBool, Boolean](JBool.apply, JBool.unapply)
    implicit object IntCodec extends SCodec[JInt, Int](n => JInt(BigInt(n)), j => Some(j.num.toInt) )
    
    
}
