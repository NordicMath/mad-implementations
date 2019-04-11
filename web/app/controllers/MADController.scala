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
    }
    
    def information() = Action { implicit request: Request[AnyContent] => 
        val infolist = memory.getInformation.map(_.toString).toList
        Ok(views.html.information(infolist))
    }
}
