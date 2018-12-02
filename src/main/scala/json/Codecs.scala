package io.github.nordicmath.mad.json

import org.json4s._

import util.Try

trait Codecs {
    // Codec helpers
    case class SCodec[S <: JValue, T](encoder : T => S, decoder : S => Option[T]) extends Codec[T] {
        def embed (j : JValue) : Option[S] = Try(j.asInstanceOf[S]).toOption
        
        def encode(t : T) = encoder(t)
        def decode(j : JValue) = for {s <- embed(j); t <- decoder(s)} yield t
    }
    
    case class TCodec[T, S : Codec](to : T => S, from : S => T) extends Codec[T] {
        def encode(t : T) = JSON[S](to(t))
        def decode(j : JValue) = JSON[S].unapply(j).map(from)
    }
    
    // Primitive codecs
    implicit object StringCodec extends SCodec[JString, String](JString.apply, JString.unapply)
    implicit object BooleanCodec extends SCodec[JBool, Boolean](JBool.apply, JBool.unapply)
    implicit object BigIntCodec extends SCodec[JInt, BigInt](JInt.apply, JInt.unapply)
    
    implicit object IntCodec extends TCodec[Int, BigInt](BigInt(_), _.toInt)
    
    // Compound codecs
    implicit def listCodec[T : Codec] = new ListCodec[T]
    class ListCodec[T : Codec] extends SCodec[JArray, List[T]] (
        lst => JArray(lst.map(JSON[T])),
        jar => jar.arr.map(JSON[T].unapply) match {
            case x if x.contains(None) => None
            case x => Some(x.flatten)
        }
    )
    
    implicit def seqCodec[T : Codec] = new SeqCodec[T]
    class SeqCodec[T : Codec] extends TCodec[Seq[T], List[T]] (_.toList, _.toSeq)
    
}
