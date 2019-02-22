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
        def encoder : PartialFunction[T, JValue]
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
    
    implicit def pairCodec[T : Codec, S : Codec] = new PairCodec[T, S]
    class PairCodec[T, S](implicit tc : Codec[T], sc : Codec[S]) extends PFCodec[(T, S)] {
        val encoder = { case (t, s) => JObject(List(JField("t1", tc(t)), JField("t2", sc(s))))}
        val decoder = { case JObject(List(JField("t1", tc(t)), JField("t2", sc(s)))) => (t, s)}
    }
    
    
    // MAD Codecs
    import structure._
    import Information._
    
    implicit def madpathcodec(implicit madtype : RichMADType) = new MADPathCodec()(madtype)
    class MADPathCodec(implicit madtype : RichMADType) extends TCodec[MADPath, Seq[String]](_.instructions, MADPath(madtype, _))
    
    implicit def informationcodec(implicit madtype : RichMADType) = new InformationCodec()(madtype)
    class InformationCodec(implicit madtype : RichMADType) extends PFCodec[Information]{
        private val MADPathCodec = madpathcodec
        private object JInfo {
            def apply(name : String, fs : List[JField]) : JValue = JObject(name -> JObject(fs))
            def unapply(j : JValue) : Option[(String, List[JField])] = j match {
                case JObject(List(JField(name, JObject(lst)))) => Some((name, lst))
                case _ => None
            }
        }
        
        private object JApply {
            def apply[T : Codec](tpe : String, path : MADPath, value : T) = JInfo("Apply", List("type" -> JString(tpe), "path" -> MADPathCodec(path), "value" -> json.encode[T](value)))
            def unapply(j : JValue) : Option[(String, JValue, MADPath)] = j match {
                case JInfo("Apply", List(JField("type", JString(tpe)), JField("path", MADPathCodec(path)), JField("value", v))) => Some((tpe, v, path))
                case _ => None
            }
        }
        
        val encoder = {
            case NoInformation => JInfo("NoInformation", List())
            case Apply(path, value : String) => JApply("String", path, value)
            case Apply(path, value : Boolean) => JApply("Boolean", path, value)
            case Apply(path, value : Int) => JApply("Int", path, value)
            case Apply(_, _) => ??? // Unreachable
            case OptionAssign(path, p) => JInfo("OptionAssign", List("path" -> MADPathCodec(path), "possible" -> JBool(p)))
            case ListNew(path) => JInfo("ListNew", List("path" -> MADPathCodec(path)))
            case MapNew(path, name) => JInfo("MapNew", List("path" -> MADPathCodec(path), "name" -> JString(name)))
            case EnumAssign(path, index) => JInfo("EnumAssign", List("path" -> MADPathCodec(path), "index" -> IntCodec(index)))
            case ReferenceApply(path, value) => JInfo("ReferenceApply", List("path" -> MADPathCodec(path), "value" -> MADPathCodec(value)))
        }
        
        val decoder = {
            case JInfo("NoInformation", List()) => NoInformation
            case JApply("String", StringCodec(vs), path) => Apply[String](path, vs)
            case JApply("Int", IntCodec(vi), path) => Apply[Int](path, vi)
            case JApply("Boolean", BooleanCodec(vb), path) => Apply[Boolean](path, vb)
            case JInfo("OptionAssign", List(JField("path", MADPathCodec(path)), JField("possible", JBool(p)))) => OptionAssign(path, p)
            case JInfo("ListNew", List(JField("path", MADPathCodec(path)))) => ListNew(path)
            case JInfo("MapNew", List(JField("path", MADPathCodec(path)), JField("name", JString(name)))) => MapNew(path, name)
            case JInfo("EnumAssign", List(JField("path", MADPathCodec(path)), JField("index", IntCodec(index)))) => EnumAssign(path, index)
            case JInfo("ReferenceApply", List(JField("path", MADPathCodec(path)), JField("value", MADPathCodec(value)))) => ReferenceApply(path, value)
        }
        
    }
}
