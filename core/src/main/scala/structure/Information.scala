package io.github.nordicmath.mad.structure

import MADNavigable._

sealed abstract class Information {
    import Information._
    
    override def toString = this match {
        case NoInformation => f"No information.."
        case Apply(path, value) => f"The attribute $path has value $value"
        case OptionAssign(path, false) => f"$path does not make sense"
        case OptionAssign(path, true) => f"$path does makes sense"
        case ListNew(path) => f"There is yet another element in $path"
        case MapNew(path, name) => f"There is something of name $name in $path"
        case EnumAssign(path, index) => f"The enum $path has index $index"
    }
    
}

object Information {
    case object NoInformation extends Information
    
    case class Apply[S](path : MADPath, value : S)(implicit val madvp : MADValuePrimitive[S]) extends Information
    case class OptionAssign(path : MADPath, possible : Boolean) extends Information
    case class ListNew(path : MADPath) extends Information
    case class MapNew(path : MADPath, name : String) extends Information
    case class EnumAssign(path : MADPath, index : Int) extends Information
    
}
