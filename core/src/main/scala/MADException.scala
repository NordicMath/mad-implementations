package io.github.nordicmath.mad

import conceptoids._

sealed abstract class MADException(msg : String) extends Exception(msg)

object MADException {
    
    abstract class InterpreterIllegalInput(correct : String) extends MADException(f"Please enter $correct!")
    object BooleanInput extends InterpreterIllegalInput("true/false")
    object IntegerInput extends InterpreterIllegalInput("an integer")
    
    case class ConceptoidNameTaken(p : String) extends MADException(f"The path-name '$p' is taken!")
    case object ConceptoidNameEmpty extends MADException(f"The path-name may not be empty!")
    
    object NoQuestions extends MADException(f"No questions found!")
    
    case class NavigationImpossible(path : MADPath, nav : MADNavigable) extends MADException(f"Navigating $path into something of type ${nav.madtype} is impossible!")
    case class MADPathMismatch(path : MADPath) extends MADException(f"In MADPath, $path did not match its own type!")
    
    case class QuestionUnsupportedType(path : MADPath, mt : MADType) extends MADException(f"No known way to ask questions about something of type $mt, at path $path!")
    
}
