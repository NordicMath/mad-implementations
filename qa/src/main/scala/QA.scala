package io.github.nordicmath.mad.qa

import io.github.nordicmath.mad._
import structure._
import questions._
import memory._
import json._

import java.nio.file.{Paths => FilePaths, Files}
import java.nio.charset.StandardCharsets

import scala.io.Source

import scala.util.Try

import org.json4s.native.JsonMethods._
import org.json4s._

import scala.reflect.runtime.universe._

case class QA()(implicit io : IO, memory : Memory, madtype : RichMADType) {
    
    private val questionEngine : QuestionEngine = new QuestionEngine()
    private val priorityEngine : PriorityEngine = new PriorityEngine(questionEngine)
    
    
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
                show("Welcome to MAD!")
                return MainMenu
            }
        }
        
        case object MainMenu extends Stage {
            def next() = Menu("Options: ",
                "Question" -> HPQuestion,
                "All questions" -> AllQuestions,
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
                "Save..." -> Menu("How do you want to save?",
                    "String" -> Save(true),
                    "File" -> Save(false)
                ),
                "Load..." -> Menu("Are you sure? This will clear current memory...",
                    "Yes..." -> Menu("How do you want to load?",
                        "String" -> Load(true),
                        "File" -> Load(false)
                    ),
                    "No, go back" -> MainMenu
                ),
                "Exit" -> Exit
            )
        }
        
        case class Menu(title : String, stages : (String, Stage)*) extends Stage {
            def next() : Stage = {
                def lookup[S](seq : Seq[S], l : String) : Option[S] = Try(seq(l.toInt - 1)).toOption
                
                show(title)
                for { ((name, stage), i) <- stages.zipWithIndex } yield show(IO.StageOption("* (" + (i + 1) + ") " + name))
                
                def enterOption() : Stage = {
                    show("Please enter option: ")
                    val opt = read()
                    lookup(stages.map(_._2), opt).getOrElse{show("No such option!"); enterOption()}
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
        
        case class Save(asString : Boolean) extends Stage {
            def next() : Stage = {
                
                val j = encode(memory.getInformation)
                
                if (asString) {
                    show("Data:")
                    show[JValue](j)
                } else {
                    show("Enter filename: ")
                    val name = read()
                    Files.write(FilePaths.get(name), compact(render(j)).getBytes(StandardCharsets.UTF_8))
                }
                
                return MainMenu
            }
        }
                
        case class Load(asString : Boolean) extends Stage {
            def next() : Stage = {
                
                val str = if(asString) {
                    show("Enter string to load:")
                    read()
                } else {
                    show("Enter filename: ")
                    val name = read()
                    Source.fromFile(name).mkString
                }
                
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
                                
                show(memory.getTree.toJSON())
                
                return MainMenu
            }
        }
        
        case object Paths extends Stage {
            def next() : Stage = {
                show("Paths: ")
                
                for (path <- priorityEngine.generatePaths()) show(path)
                
                return MainMenu
            }
        }
        
        case object InformationList extends Stage {
            def next() : Stage = {
                show("List of information: ")
                
                memory.getInformation.foreach(show[Information])
                
                return MainMenu
            }
        }
        
        case object FailedInformationList extends Stage {
            def next() : Stage = {
                show("List of failed information: ")
                
                memory.getFailedInformation.foreach(show[Information])
                
                return MainMenu
            }
        }
        
        case object AllQuestions extends Stage {
            def next() : Stage = {
                val questions = priorityEngine.generateQuestions()
                val questionText = questions.map (_.text)
                val questionMenu = questions.map (QuestionS.apply)
                
                return Menu("Questions", ((questionText zip questionMenu) :+ ("Back" -> MainMenu)) : _*)
            }
        }
        
        case object HPQuestion extends Stage {
            def next() : Stage = {
                return QuestionS(priorityEngine.generateQuestion())
            }
        }
        
        
        case class QuestionS(question : Question) extends Stage {
            
            def next() : Stage = {
                
                try {
                    show(question.text)
                    
                    val ans = read()
                    val info = question.interpreter.interpret(ans)
                    
                    memory.push(info)
                } catch {
                    case ex : MADException => show(ex.toString)
                }
                
                return MainMenu
            }
        }
    }
    
    def start() = {
        var cstage : Stage = Stage.Intro
        while (cstage != Stage.Exit) cstage = cstage.next()
        io.show("Bye!")
    }
}
