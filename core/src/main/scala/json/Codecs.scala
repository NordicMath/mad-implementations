package io.github.nordicmath.mad.json

import io.github.nordicmath.mad._

import org.json4s._

import util.Try

import scala.reflect.ClassTag

import scala.language.implicitConversions

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
    
    implicit def subPFCodec[T : ClassTag, S >: T](codec : T) : PFCodec[S] = new SubPFCodec()(codec, implicitly[ClassTag[T]])
    class SubPFCodec[T : Codec : ClassTag, S >: T] extends PFCodec[S] {
        lazy val codec = implicitly[Codec[T]]
        lazy val encoder = { case x : T => codec(x) }
        lazy val decoder = { case codec(x : T) => x }
    }
    
    class SingletonCodec[T](t : T, j : JValue) extends PFCodec[T] {
        lazy val encoder = {case `t` => j}
        lazy val decoder = {case `j` => t}
    }
    
    class UnionCodec[T] (codecs : PFCodec[T]*) extends PFCodec[T] {
        def encoder = codecs.map(_.encoder).reduceLeft(_ orElse _)
        def decoder = codecs.map(_.decoder).reduceLeft(_ orElse _)
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
    
    case class AnyCodec[T](c : Codec[T]) {
        def apply (x : Any) = c.apply(x.asInstanceOf[T])
        def unapply (j : JValue) = c.unapply(j)
    }
    private class NamedListCodec (params : (String, Codec[_])*) extends Codec[Seq[Any]] {
        private val paramsA : Seq[(String, AnyCodec[_])] = params.map{case (str, codec) => (str, AnyCodec(codec))}
        def encode (seq : Seq[Any]) = JObject((paramsA zip seq).map{case ((param, codec), ob) => JField(param, codec(ob))} : _*)
        
        def decode (jval : JValue) = Try{
            jval match {
                case JObject(fields) => (for {
                    ((p1, codec), (p2, j)) <- paramsA zip fields
                    if p1 == p2
                    ob <- codec.unapply(j)
                } yield ob) match {
                    case x if x.length == fields.length => Some(x)
                    case _ => None
                }
                
                case _ => None
            }
        }.toOption.flatten
    }
    
    object NamedTuples {
        sealed class NamedTupleGenericCodec[T <: Product](params : (String, Codec[_])*) extends Codec[T] {
            private val inner = new NamedListCodec(params : _*)
            def encode (t : T) = inner.encode(t.productIterator.toSeq)
            def decode (j : JValue) = inner.decode(j).map(toTuple(_).asInstanceOf[T])
            
            def toTuple (s : Seq[Any]) : Product = s.size match {
                case 1 => Tuple1(s(0))
                case 2 => Tuple2(s(0), s(1))
                case 3 => Tuple3(s(0), s(1), s(2))
                case 4 => Tuple4(s(0), s(1), s(2), s(3))
                case 5 => Tuple5(s(0), s(1), s(2), s(3), s(4))
                case _ => ???
            }
        }
        
        class NT1Codec[T](name1 : String)(implicit codec1 : Codec[T]) extends NamedTupleGenericCodec[Tuple1[T]]((name1, codec1))
        class NT2Codec[T, S](name1 : String, name2 : String)(implicit codec1 : Codec[T], codec2 : Codec[S]) extends NamedTupleGenericCodec[Tuple2[T, S]]((name1, codec1), (name2, codec2))
        class NT3Codec[T, S, U](name1 : String, name2 : String, name3 : String)(implicit codec1 : Codec[T], codec2 : Codec[S], codec3 : Codec[U]) extends NamedTupleGenericCodec[Tuple3[T, S, U]]((name1, codec1), (name2, codec2), (name3, codec3))
        class NT4Codec[T, S, U, V](name1 : String, name2 : String, name3 : String, name4 : String)(implicit codec1 : Codec[T], codec2 : Codec[S], codec3 : Codec[U], codec4 : Codec[V]) extends NamedTupleGenericCodec[Tuple4[T, S, U, V]]((name1, codec1), (name2, codec2), (name3, codec3), (name4, codec4))
        class NT5Codec[T, S, U, V, W](name1 : String, name2 : String, name3 : String, name4 : String, name5 : String)(implicit codec1 : Codec[T], codec2 : Codec[S], codec3 : Codec[U], codec4 : Codec[V], codec5 : Codec[W]) extends NamedTupleGenericCodec[Tuple5[T, S, U, V, W]]((name1, codec1), (name2, codec2), (name3, codec3), (name4, codec4), (name5, codec5))
        
    }
    
    import NamedTuples._
    
    // MAD Codecs
    import structure._
    import MADPath._
    import MADType._
    import Information._
    
    implicit object MADTypeCodec extends PFCodec[MADType]{
        lazy val paramsCodec = seqCodec(pairCodec(StringCodec, RichMADTypeCodec))
        lazy val encoder = {
            case MADString => JObject(List(JField("type", JString("string"))))
            case MADBool => JObject(List(JField("type", JString("bool"))))
            case MADInt => JObject(List(JField("type", JString("int"))))
            case MADTree(name, params @ _*) => JObject(List(JField("type", JString("tree")), JField("name", JString(name)), JField("params", paramsCodec(params))))
            case MADList(param) => JObject(List(JField("type", JString("list")), JField("param", RichMADTypeCodec(param))))
            case MADOption(param) => JObject(List(JField("type", JString("option")), JField("param", RichMADTypeCodec(param))))
        }
        lazy val decoder = {
            case JObject(List(JField("type", JString("string")))) => MADString
            case JObject(List(JField("type", JString("bool")))) => MADBool
            case JObject(List(JField("type", JString("int")))) => MADInt
            case JObject(List(JField("type", JString("tree")), JField("name", JString(name)), JField("params", paramsCodec(params)))) => MADTree(name, params : _*)
            case JObject(List(JField("type", JString("list")), JField("param", RichMADTypeCodec(param)))) => MADList(param)
            case JObject(List(JField("type", JString("option")), JField("param", RichMADTypeCodec(param)))) => MADOption(param)
        }
    }
    
    implicit object RichMADTypeCodec extends TCodec[RichMADType, (MADType, Int)](RichMADType.unapply(_).get, RichMADType.tupled)(new NT2Codec[MADType, Int]("madtype", "priority"))
    
    
    implicit object MADPathCodec extends PFCodec[MADPath]{
        val encoder = {
            case Destination(madtype) => JObject("dest" -> RichMADTypeCodec(madtype)) 
            case EnterTree(param, next, madtype) => JObject("param" -> JString(param), "type" -> RichMADTypeCodec(madtype), "next" -> MADPathCodec(next))
            case EnterList(index, next, madtype) => JObject("index" -> JInt(index), "type" -> RichMADTypeCodec(madtype), "next" -> MADPathCodec(next))
            case EnterOption(next, madtype) => JObject("enter" -> JObject(), "type" -> RichMADTypeCodec(madtype), "next" -> MADPathCodec(next))
        }
        val decoder = {
            case JObject(List(JField("dest", RichMADTypeCodec(madtype)))) => Destination(madtype)
            case JObject(List(JField("param", JString(param)), JField("type", RichMADTypeCodec(madtype)), JField("next", MADPathCodec(next)))) => EnterTree(param, next, madtype)
            case JObject(List(JField("index", JInt(index)), JField("type", RichMADTypeCodec(madtype)), JField("next", MADPathCodec(next)))) => EnterList(index.toInt, next, madtype)
            case JObject(List(JField("enter", JObject(List())), JField("type", RichMADTypeCodec(madtype)), JField("next", MADPathCodec(next)))) => EnterOption(next, madtype)
        }
    }
    
    implicit object InformationCodec extends PFCodec[Information]{
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
        }
        
        val decoder = {
            case JInfo("NoInformation", List()) => NoInformation
            case JApply("String", StringCodec(vs), path) => Apply[String](path, vs)
            case JApply("Int", IntCodec(vi), path) => Apply[Int](path, vi)
            case JApply("Boolean", BooleanCodec(vb), path) => Apply[Boolean](path, vb)
            case JInfo("OptionAssign", List(JField("path", MADPathCodec(path)), JField("possible", JBool(p)))) => OptionAssign(path, p)
            case JInfo("ListNew", List(JField("path", MADPathCodec(path)))) => ListNew(path)
        }
        
    }
}
