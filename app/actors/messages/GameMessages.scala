package actors.messages

import play.api.libs.json._
import actors.state.Block

object GameMessages {

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

  case object UnsupportedMessage extends Message {
    val playerName = None
    val kind = "unsupported"
  }

  case class BlockMoved(player: String, direction: MoveDirection) extends Message {
    val playerName = Some(player)
    val kind = "blockMoved"
  }

  case class Move(player: String, direction: MoveDirection) extends Message {
    val playerName = Some(player)
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

  case class PlayerEvent(payload: JsValue)

  case class BlockEmbedded(player: String, newBlock: Block) extends Message {
    val playerName = Some(player)
    val kind = BlockEmbedded.kind
  }

  object BlockEmbedded {
    type BlockShape = Int
    val kind = "embedded"

    implicit val jsonFormat: Writes[BlockEmbedded] = new Writes[BlockEmbedded] {
      override def writes(be: BlockEmbedded): JsValue = {
        val payload = JsObject(Seq(
          ("newBlock" -> JsObject(Seq(
            ("shape" -> JsNumber(be.newBlock.origShape))
          )))
        ))
        Message.json(kind, be.playerName, payload)
      }
    }

  }

}
