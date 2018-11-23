package io.github.nordicmath.mad

import memory._
import scala.reflect.runtime.universe._

case class QA()(implicit io : IO, memory : Memory) {
    
    trait Stage {
        def next() : Stage
    }
    
    object Stage {
        private def show[S : TypeTag](ob : S) = io.show[S](ob)
        private def read() = io.read()
    
        private val introOptionsStage = ShowStages("New conceptoid" -> NewConceptoid, "Question" -> Question, "Display memory" -> Display, "List paths" -> Paths, "Show information list" -> InformationList, "Exit" -> Exit)
    
        case object Exit extends Stage {
            def next() : Stage = {
                return Exit
            }
        }
        
        case object Intro extends Stage {
            def next() : Stage = {
                show("Welcome to Zophie!")
                return introOptionsStage
            }
        }
        
        case object Display extends Stage {
            def next() : Stage = {
                show("Current state: ")

                // TODO: Improve using locks
                Thread.sleep(500)
                
                memory.getObjects.foreach { case (name, conceptoid) =>
                    show(name + ": " + conceptoid)
                }
                
                return introOptionsStage
            }
        }
        
        case object Paths extends Stage {
            def next() : Stage = {
                show("Paths: ")
                
                import questions._
                
                // TODO: Improve using locks
                Thread.sleep(500)
                
                for (path <- PriorityEngine.generatePaths()) show(path)
                
                return introOptionsStage
            }
        }
        
        case class ShowStages(stages : (String, Stage)*) extends Stage {
            def next() : Stage = {
                def lookup[S](lst : List[(String, S)], l : String) : Option[S] = lst match {
                    case (l1, s) :: _ if l1.head.toLower == l.head.toLower => Some(s) 
                    case Seq() => None
                    case _ :: tail => lookup(tail, l)
                }
                
                show("Options:")
                for { (name, stage) <- stages } yield show(IO.StageOption("* (" + name.head.toLower + ") " + name))
                
                def enterOption() : Stage = {
                    show("Please enter option: ")
                    val opt = read()
                    lookup(stages.toList, opt).getOrElse{show("No such option!"); enterOption()}
                }
                
                return enterOption()
            }
        }
        
        case object InformationList extends Stage {
            def next() : Stage = {
                show("List of information: ")
                
                memory.getInformation.foreach(show)
                
                return introOptionsStage
            }
        }
        
        case object Question extends Stage {
            import questions._
            import conceptoids._
            
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
                
                return introOptionsStage
            }
        }
        
        case object NewConceptoid extends Stage {
            def next() : Stage = {
                import conceptoids._
                show("What is the path-name of this concept?")
                val name = read()
                
                // TODO: Implement in Question
                memory.push(Information.NewConceptoid(name))
                
                return introOptionsStage
                
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
