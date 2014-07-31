package actors.messages

import play.api.libs.json.{JsValue, JsString, JsObject}

object Message {
  def json[T](kind: String, player: String, payload: JsValue): JsValue =
    JsObject(Seq(
      ("kind" -> JsString(kind)),
      ("player" -> JsString(player)),
      ("payload" -> payload)
    ))
}

trait Message {
  def kind: String

  def playerName: String
}