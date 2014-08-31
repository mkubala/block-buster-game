package actors.messages

import play.api.libs.json.{JsNull, JsValue, JsString, JsObject}

object Message {
  def json[T](kind: String, player: Option[String], payload: JsValue): JsValue =
    JsObject(Seq(
      ("kind" -> JsString(kind)),
      ("player" -> player.map(JsString).getOrElse(JsNull)),
      ("payload" -> payload)
    ))
}

trait Message {
  def kind: String

  def playerName: Option[String]
}