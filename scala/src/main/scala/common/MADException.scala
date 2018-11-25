package io.github.nordicmath.mad

abstract class MADException(msg : String) extends Exception(msg)

object MADException {
	import conceptoids._
	
	abstract class InterpreterIllegalInput(correct : String) extends MADException(f"Please enter $correct!")
	object BooleanInput extends InterpreterIllegalInput("true/false")
	object IntegerInput extends InterpreterIllegalInput("an integer")
	
	case class ConceptoidNameTaken(p : String) extends MADException(f"The path-name $p is taken!")
	case class InformationUnhandled(i : Information, memsys : String) extends MADException(f"$memsys can't handle the following information: $i")
	
	object MADValueUnsuppertedType extends MADException(f"Unsupported type passed to MADValue!")
	
	object NoQuestions extends MADException(f"No questions found!")
	
	case class NavigationImpossible(path : MADPath, nav : MADNavigable[_]) extends MADException(f"Navigating $path into something of type ${nav.madtype} is impossible!")
	
}
