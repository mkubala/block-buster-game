package actors.messages

import play.api.libs.json._

object GameMessages {

  case class Tick(playerName: String) extends Message {
    val kind = "tick"
  }

  object Tick {
    implicit val jsonWrites: Writes[Tick] = new Writes[Tick] {
      override def writes(tick: Tick): JsValue =
        Message.json("tick", tick.playerName, JsObject(Seq.empty[(String, JsValue)]))
    }
  }

  sealed class MoveDirection(val encodedVal: Int)

  case object MoveLeft extends MoveDirection(-1)

  case object MoveDown extends MoveDirection(0)

  case object MoveRight extends MoveDirection(1)

  object MoveDirection {
    def apply(encodedValue: Int): MoveDirection =
      if (encodedValue == -1) {
        MoveLeft
      } else if (encodedValue == 1) {
        MoveRight
      } else {
        MoveDown
      }
  }

  case class Move(playerName: String, direction: MoveDirection) extends Message {
    val kind = Move.kind
  }

  object Move {
    val kind = "move"

    implicit val jsonFormat: Format[Move] = new Format[Move] {

      override def writes(move: Move): JsValue = Message.json(Move.kind, move.playerName, JsObject(Seq(
        ("direction" -> JsNumber(move.direction.encodedVal))
      )))

      override def reads(json: JsValue): JsResult[Move] = JsSuccess {
        val player = (json \ "player").as[String]
        val direction = MoveDirection((json \ "payload" \ "direction").as[Int])
        Move(player, direction)
      }
    }
  }

  case class PlayerEvent(event: JsValue)

  case class BlockEmbedded(playerName: String, blockShape: BlockEmbedded.BlockShape) extends Message {
    val kind = BlockEmbedded.kind
  }

  object BlockEmbedded {
    type BlockShape = Int
    val kind = "embedded"

    implicit val jsonFormat: Format[BlockEmbedded] = new Format[BlockEmbedded] {
      override def writes(be: BlockEmbedded): JsValue = {
        val payload = JsObject(Seq(
          ("newBlock" -> JsObject(Seq(
            ("shape" -> JsNumber(be.blockShape))
          )))
        ))
        Message.json(kind, be.playerName, payload)
      }

      override def reads(json: JsValue): JsResult[BlockEmbedded] =
        (for {
          kind <- (json \ "kind").asOpt[String]
          if (kind == BlockEmbedded.kind)
          player <- (json \ "player").asOpt[String]
          shape <- (json \ "payload" \ "newBlock" \ "shape").asOpt[BlockShape]
        } yield BlockEmbedded(player, shape)).map(JsSuccess(_)).getOrElse(JsError())
    }

  }

}
