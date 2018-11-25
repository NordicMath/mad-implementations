package io.github.nordicmath.mad.conceptoids

import scala.reflect.runtime.universe._


sealed abstract class Information {
    import Information._
    
    override def toString = this match {
        case NoInformation => f"No information.."
        case NewConceptoid(name) => f"There is a conceptoid with name $name"
        case Apply(path, value) => f"The attribute $path has value $value"
        case OptionAssign(path, false) => f"$path does not make sense"
        case OptionAssign(path, true) => f"$path does makes sense"
        case ListNew(path) => f"There is yet another element in $path"
    }
}

object Information {    
    case object NoInformation extends Information
    
    case class NewConceptoid(name : String) extends Information
    case class Apply[S](path : Path, value : S)(implicit val typetag : TypeTag[S]) extends Information
    case class OptionAssign(path : Path, possible : Boolean) extends Information
    case class ListNew(path : Path) extends Information
}
