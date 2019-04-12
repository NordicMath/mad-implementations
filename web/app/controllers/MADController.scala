package controllers

import javax.inject._
import play.api._
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

    val madtype = Conceptoid.Conceptoids
    val mema : MemoryAccess = MemoryAccess(madtype)
    implicit val memory : Memory = mema.getMemory
    implicit val api : APIInstance = new APIInstance()

    def index() = Action { implicit request: Request[AnyContent] =>
        Ok(views.html.index())
    }

    def treeNoPath = tree("mad://")
    def tree(pathtext : String) = Action { implicit request: Request[AnyContent] => 
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
    
    def newconceptoid = answer("mad://")
    def answer(pathtext : String) = Action { implicit request: Request[AnyContent] => 
        val path : MADPath = Interpreter.parseMADPath(pathtext, madtype)
        val questions : Seq[Question] = api.questions(path)
        Ok(views.html.answer(path.toString, questions.map(_.text)))
    }
    
    def information() = Action { implicit request: Request[AnyContent] => 
        val infolist = memory.getInformation.map(_.toString).toList
        Ok(views.html.information(infolist))
    }
}
