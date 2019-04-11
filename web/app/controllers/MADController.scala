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


    def index() = Action { implicit request: Request[AnyContent] =>
        Ok(views.html.index())
    }

}
