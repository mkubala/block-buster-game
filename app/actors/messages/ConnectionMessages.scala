package actors.messages

import play.api.libs.iteratee.Enumerator
import play.api.libs.json.JsValue

object ConnectionMessages {

  case class Join(username: String)

  case class Connected(enumerator: Enumerator[JsValue])

  case class ConnectionError(cause: String)

  case class Disconnected(playerName: String)

}
