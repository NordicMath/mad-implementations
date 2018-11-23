package io.github.nordicmath.mad

import memory._

case class QA()(implicit io : IO, memory : Memory) {
    
    trait Stage {
        def next() : Stage
    }
    
    object Stage {
        private def show(str : String) = io.show(str)
        private def read() = io.read()
    
        private val introOptionsStage = ???
    
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
                println("Current state: ")
                import org.json4s.native.JsonMethods._

                // TODO: Improve using locks
                Thread.sleep(500)

                memory.getObjects.foreach { case (name, conceptoid) =>
                    println(name + ": " + pretty(render(conceptoid.toJSON())))
                }
                
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
                for { (name, stage) <- stages } yield show("* (" + name.head.toLower + ") " + name)
                
                def enterOption() : Stage = {
                    show("Please enter option: ")
                    val opt = read()
                    lookup(stages.toList, opt).getOrElse{show("No such option!"); enterOption()}
                }
                
                return enterOption()
            }
        }
        
        case object Question extends Stage {
            import questions._
            import conceptoids._
            
            def next() : Stage = {
                return introOptionsStage
            }
        }
        
        case object NewConceptoid extends Stage {
            def next() : Stage = {
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
