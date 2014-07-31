package controllers

import actors.BlockBusterGameActor
import play.api.libs.json.JsValue
import play.api.mvc._

object Application extends Controller {

  def index = Action { implicit request =>
    Ok(views.html.index())
  }

  def game(playerName: String) = Action { implicit request =>
    Ok(views.html.game(playerName))
  }

  def gameSocket(playerName: String) = WebSocket.tryAccept[JsValue] { request =>
    BlockBusterGameActor.join(playerName)
  }

}