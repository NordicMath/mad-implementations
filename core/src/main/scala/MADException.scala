package io.github.nordicmath.mad

import structure._

sealed abstract class MADException(msg : String) extends Exception(msg)

object MADException {
    
    abstract class InterpreterIllegalInput(correct : String) extends MADException(f"Please enter $correct!")
    object BooleanInput extends InterpreterIllegalInput("true/yes/false/no")
    object IntegerInput extends InterpreterIllegalInput("an integer")
    object MADPathInput extends InterpreterIllegalInput("a MAD-path!")
    
    object SchemaFailMADPath extends MADException("This MAD-path does not fit the schema!")
    object PredicateFailMADPath extends MADException("This MAD-path does not satisfy a predicate!")
    object UndefinedMADPath extends MADException("This MAD-path does not exist!")
    
    case class MapNameInUse(name : String) extends MADException(f"The name '$name' being added to a map is already in use!")
    
    object NoQuestions extends MADException(f"No questions found!")
    
    case class MADPathCollision(seq : Seq[String]) extends MADException(f"MADPath name collision in ${seq.find(_.contains("/"))}!")
    case class NavigationImpossible(path : MADPath, nav : MADNavigable) extends MADException(f"Navigating $path into something of type ${nav.madtype} is impossible!")
    case class MADPathMismatch(path : MADPath) extends MADException(f"In MADPath, $path did not match its own type!")
    
    case class MADTypeNotIterable(madtype : MADType) extends MADException(f"MADType $madtype is not iterable!")
    
}
