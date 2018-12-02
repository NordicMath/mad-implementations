package io.github.nordicmath.mad.json

import io.github.nordicmath.mad._

import org.json4s._

import util.Try

trait Codecs {
    // Codec helpers
    class SCodec[S <: JValue, T](encoder : T => S, decoder : S => Option[T]) extends Codec[T] {
        def embed (j : JValue) : Option[S] = Try(j.asInstanceOf[S]).toOption
        
        def encode(t : T) = encoder(t)
        def decode(j : JValue) = for {s <- embed(j); t <- decoder(s)} yield t
    }
    
    class TCodec[T, S : Codec](to : T => S, from : S => T) extends Codec[T] {
        def encode(t : T) = json.encode[S](to(t))
        def decode(j : JValue) = json.decode[S](j).map(from)
    }
    
    abstract class PFCodec[T] extends Codec[T] {
        def encoder : (T => JValue)
        def decoder : PartialFunction[JValue, T]
        
        final def encode(t : T) = encoder(t)
        final def decode(j : JValue) = decoder.lift(j)
    }
    
    // Primitive codecs
    implicit object StringCodec extends SCodec[JString, String](JString.apply, JString.unapply)
    implicit object BooleanCodec extends SCodec[JBool, Boolean](JBool.apply, JBool.unapply)
    implicit object BigIntCodec extends SCodec[JInt, BigInt](JInt.apply, JInt.unapply)
    
    implicit object IntCodec extends TCodec[Int, BigInt](BigInt(_), _.toInt)
    
    // Compound codecs
    implicit def listCodec[T : Codec] = new ListCodec[T]
    class ListCodec[T : Codec] extends SCodec[JArray, List[T]] (
        lst => JArray(lst.map(json.encode[T])),
        jar => jar.arr.map(json.decode[T]) match {
            case x if x.contains(None) => None
            case x => Some(x.flatten)
        }
    )
    
    implicit def seqCodec[T : Codec] = new SeqCodec[T]
    class SeqCodec[T : Codec] extends TCodec[Seq[T], List[T]] (_.toList, _.toSeq)
    
    // MAD Codecs
    import io.github.nordicmath.mad.conceptoids._
    import MADPath._
    
    implicit object MADPathCodec extends PFCodec[MADPath]{
        val encoder = {
            case Destination => JString("Destination")
            case EnterTree(param, next) => JObject("param" -> JString(param), "next" -> MADPathCodec(next))
            case EnterList(index, next) => JObject("index" -> JInt(index), "next" -> MADPathCodec(next))
            case EnterOption(next) => JObject("enter" -> JObject(), "next" -> MADPathCodec(next))
        }
        val decoder = {
            case JString("Destination") => Destination
            case JObject(List(JField("param", JString(param)), JField("next", MADPathCodec(next)))) => EnterTree(param, next)
            case JObject(List(JField("index", JInt(index)), JField("next", MADPathCodec(next)))) => EnterList(index.toInt, next)
            case JObject(List(JField("enter", JObject(List())), JField("next", MADPathCodec(next)))) => EnterOption(next)
        }
    }
    
    implicit object PathCodec extends PFCodec[Path]{
        val encoder = { case Path(cname, mpath) => JObject(
                "cname" -> JString(cname),
                "mpath" -> MADPathCodec(mpath)
        )}
        val decoder = { case JObject(List(
                JField("cname", JString(cname)),
                JField("mpath", MADPathCodec(mpath))
        )) => Path(cname, mpath)}
    }
}
