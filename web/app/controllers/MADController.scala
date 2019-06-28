package controllers

import javax.inject._
import play.api.mvc._
import play.twirl.api._

import io.github.nordicmath.mad._
import memory._
import structure._
import spec._
import interpretation._
import questions._
import web.api._


@Singleton
class MADController @Inject()(cc: ControllerComponents) extends AbstractController(cc) {

    val spec = Conceptoid
    val madtype = spec.top
    val mema : MemoryAccess = MemoryAccess(spec)
    implicit val memory : Memory = mema.getMemory
    implicit val api : APIInstance = new APIInstance()

    def index() = Action {
        Ok(views.html.index())
    }

    def treeNoPath = tree("mad://")
    def tree(pathtext : String) = Action {
        val path : MADPath = Interpreter.parseMADPath(pathtext, madtype)
        val elements : Seq[TreeElement] = api.get(path)
        import TreeElement._
        val htmls : Seq[Html] = elements.map{
            case Value(name, None) => views.html.tree.value(name, "No Value")
            case Value(name, Some(value)) => views.html.tree.value(name, value.toString)
            case Link(name, path) => views.html.tree.link(name, path.toString)
            case Ref(name, None) => views.html.tree.value(name, "No Value")
            case Ref(name, Some(path)) => views.html.tree.refvalue(name, path.toString)
        }
        Ok(views.html.tree.page(path.toString, htmls))
    }
    
    def newconceptoid = questionsabout("mad://")
    def questionsabout(pathtext : String) = Action {
        val path : MADPath = Interpreter.parseMADPath(pathtext, madtype)
        val questions : Seq[Question] = api.questions(path)
        Ok(views.html.questions(questions.map(_.text)))
    }
    
    def information() = Action {
        val infolist = memory.getInformation.map(_.toString).toList
        Ok(views.html.information(infolist))
    }
    
    def questions() = Action {
        val questions = api.questions()
        Ok(views.html.questions(questions.map(_.text)))
    }
}
