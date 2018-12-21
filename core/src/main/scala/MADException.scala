package io.github.nordicmath.mad

import structure._

sealed abstract class MADException(msg : String) extends Exception(msg)

object MADException {
    
    abstract class InterpreterIllegalInput(correct : String) extends MADException(f"Please enter $correct!")
    object BooleanInput extends InterpreterIllegalInput("true/yes/false/no")
    object IntegerInput extends InterpreterIllegalInput("an integer")
    
    case class MapNameInUse(name : String) extends MADException(f"The name '$name' being added to a map is already in use!")
    
    object NoQuestions extends MADException(f"No questions found!")
    
    case class NavigationImpossible(path : MADPath, nav : MADNavigable) extends MADException(f"Navigating $path into something of type ${nav.madtype} is impossible!")
    case class MADPathMismatch(path : MADPath) extends MADException(f"In MADPath, $path did not match its own type!")
    
}
