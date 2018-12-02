package io.github.nordicmath.mad

import memory._
import scala.reflect.runtime.universe._

case class QA()(implicit io : IO, memory : Memory) {
    
    sealed trait Stage {
        def next() : Stage
    }
    
    object Stage {
        private def show[S : TypeTag](ob : S) = io.show[S](ob)
        private def read() = io.read()
    
    
        case object Exit extends Stage {
            def next() : Stage = {
                return Exit
            }
        }
        
        case object Intro extends Stage {
            def next() : Stage = {
                show("Welcome to Zophie!")
                return MainMenu
            }
        }
        
        case object MainMenu extends Stage {
            def next() = Menu("Options: ",
                "New conceptoid" -> NewConceptoid,
                "Question" -> Question,
                "Display..." -> Menu("Display...", 
                    "Memory" -> Display,
                    "Paths" -> Paths,
                    "Information list" -> InformationList,
                    "Failed information" -> FailedInformationList,
                    "Back" -> MainMenu
                ),
                "Clear memory..." -> Menu("Are you sure?",
                    "Yes" -> ClearMemory, 
                    "No, go back" -> MainMenu
                ), 
                "Save" -> Save,
                "Load" -> Load,
                "Exit" -> Exit
            )
        } 
        
        case class Menu(title : String, stages : (String, Stage)*) extends Stage {
            def next() : Stage = {
                def lookup[S](lst : List[(String, S)], l : String) : Option[S] = lst match {
                    case (l1, s) :: _ if l1.head.toLower == l.head.toLower => Some(s) 
                    case Seq() => None
                    case _ :: tail => lookup(tail, l)
                }
                
                show(title)
                for { (name, stage) <- stages } yield show(IO.StageOption("* (" + name.head.toLower + ") " + name))
                
                def enterOption() : Stage = {
                    show("Please enter option: ")
                    val opt = read()
                    lookup(stages.toList, opt).getOrElse{show("No such option!"); enterOption()}
                }
                
                return enterOption()
            }
        }
        
        case object ClearMemory extends Stage {
            def next() : Stage = {
                show("Clearing meory...")
                
                memory.reset()
                
                return MainMenu
            }
        }
        
        case object Save extends Stage {
            def next() : Stage = {
                import json._
                import org.json4s._
                
                show("Data:")
                show[JValue](encode(memory.getInformation))
                
                return MainMenu
            }
        }
                
        case object Load extends Stage {
            def next() : Stage = {
                import json._
                import org.json4s._
                import org.json4s.native.JsonMethods._
                import conceptoids._
                
                show("Enter string to load:")
                val str = read()
                val j = parse(str)
                val data = decode[Seq[Information]](j).get
                
                memory.reset()
                data foreach memory.push _
                 
                
                
                return MainMenu
            }
        }
        
        case object Display extends Stage {
            def next() : Stage = {
                show("Current state: ")
                
                memory.getObjects.foreach { case (name, conceptoid) =>
                    show(name + ": " + conceptoid)
                }
                
                return MainMenu
            }
        }
        
        case object Paths extends Stage {
            def next() : Stage = {
                show("Paths: ")
                
                import questions._
                
                for (path <- PriorityEngine.generatePaths()) show(path)
                
                return MainMenu
            }
        }
        
        case object InformationList extends Stage {
            def next() : Stage = {
                show("List of information: ")
                
                import conceptoids._
                memory.getInformation.foreach(show[Information])
                
                return MainMenu
            }
        }
        
        case object FailedInformationList extends Stage {
            def next() : Stage = {
                show("List of failed information: ")
                
                import conceptoids._
                memory.getFailedInformation.foreach(show[Information])
                
                return MainMenu
            }
        }
        
        case object Question extends Stage {
            import questions._
            
            def next() : Stage = {
                
                try {
                    val path = PriorityEngine.generatePath()
                    val question = QuestionEngine.question(path)
                    
                    show(question.text)
                    
                    val ans = read()
                    val info = question.interpreter.interpret(question.path, ans)
                    
                    memory.push(info)
                } catch {
                    case ex : MADException => show(ex.toString)
                }
                
                return MainMenu
            }
        }
        
        case object NewConceptoid extends Stage {
            def next() : Stage = {
                import conceptoids._
                show("What is the path-name of this concept?")
                val name = read()
                
                try memory.push(Information.NewConceptoid(name)) catch {
                    case ex : MADException => show(ex.toString)
                }
                
                return MainMenu
                
            }
        }
    }
    
    def start() = {
        var cstage : Stage = Stage.Intro
        while(cstage != Stage.Exit){
            cstage = cstage.next()
        }
        io.show("Bye!")
    }    
}
