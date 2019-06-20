package io.github.nordicmath.mad.structure

import io.github.nordicmath.mad._
import MADNavigable._

sealed abstract class Information {
    import Information._
    
    override def toString = this match {
        case Apply(path, value) => f"The attribute $path has value $value"
        case OptionAssign(path, false) => f"$path does not make sense"
        case OptionAssign(path, true) => f"$path does makes sense"
        case ListNew(path) => f"There is yet another element in $path"
        case MapNew(path, name) => f"There is something of name $name in $path"
        case EnumAssign(path, index) => f"The enum $path has index $index"
        case ReferenceApply(path, value) => f"The reference $path points to $value"
        case ListStop(path) => f"The list $path is finished"
        case MapStop(path) => f"The map $path is finished"
    }
    
}

object Information {
    import MADType._
    
    case class Apply[S](path : MADPath, value : S)(implicit val madvp : MADValuePrimitive[S]) extends Information {
        if (path.madtype.inner != madvp.madtype.inner) throw MADException.InformationTypeMismatch(path, madvp.madtype.name)
    }
    
    case class OptionAssign(path : MADPath, possible : Boolean) extends Information {
        if (!path.madtype.inner.isInstanceOf[MADOption]) throw MADException.InformationTypeMismatch(path, "MADOption")
    }
    
    case class ListNew(path : MADPath) extends Information {
        if (!path.madtype.inner.isInstanceOf[MADList]) throw MADException.InformationTypeMismatch(path, "MADList")
    }
    
    case class MapNew(path : MADPath, name : String) extends Information {
        if (!path.madtype.inner.isInstanceOf[MADMap]) throw MADException.InformationTypeMismatch(path, "MADMap")
    }
    
    case class EnumAssign(path : MADPath, index : Int) extends Information {
        if (!path.madtype.inner.isInstanceOf[MADEnum]) throw MADException.InformationTypeMismatch(path, "MADEnum")
    }
    
    case class ReferenceApply(path : MADPath, value : MADPath) extends Information {
        if (!path.madtype.inner.isInstanceOf[MADRef]) throw MADException.InformationTypeMismatch(path, "MADRef")
    }
    
    case class ListStop(path : MADPath) extends Information {
        if (!path.madtype.inner.isInstanceOf[MADList]) throw MADException.InformationTypeMismatch(path, "MADList")
    }
    
    case class MapStop(path : MADPath) extends Information {
        if (!path.madtype.inner.isInstanceOf[MADMap]) throw MADException.InformationTypeMismatch(path, "MADMap")
    }
}
