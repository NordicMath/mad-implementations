package io.github.nordicmath.mad

sealed abstract class MADException(msg : String) extends Exception(msg)

object MADException {
	import conceptoids._
	
	abstract class InterpreterIllegalInput(correct : String) extends MADException(f"Please enter $correct!")
	object BooleanInput extends InterpreterIllegalInput("true/false")
	object IntegerInput extends InterpreterIllegalInput("an integer")
	
	case class ConceptoidNameTaken(p : String) extends MADException(f"The path-name '$p' is taken!")
	
	object MADValueUnsuppertedType extends MADException(f"Unsupported type passed to MADValue!")
	object InformationJSONUnsupportedType extends MADException(f"Cannot JSON the application of an unsupported type!")
	
	object NoQuestions extends MADException(f"No questions found!")
	
	case class NavigationImpossible(path : MADPath, nav : MADNavigable[_]) extends MADException(f"Navigating $path into something of type ${nav.madtype} is impossible!")
	
	case class QuestionUnsupportedType(path : Path, mt : MADType) extends MADException(f"No known way to ask questions about something of type $mt, at path $path!")

}
