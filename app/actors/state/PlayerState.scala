package actors.state

import scala.concurrent.duration._

case class PlayerState(tickInterval: FiniteDuration,
                       block: Block,
                       board: Board) {

  def moveBlockDown: (PlayerState, Option[Block]) =
    if (block.isEmbed(board)) {
      val newBlock = Block.random
      (copy(board = board.embed(block), block = newBlock), Some(newBlock))
    } else (copy(block = block.oneRowDown), None)

}

object PlayerState {

  val defaultTickInterval: FiniteDuration = 500 milliseconds

  lazy val defaultBoard = Board.empty(10, 20)

  def withBlock(block: Block): PlayerState = PlayerState(defaultTickInterval, Block.random, defaultBoard)

}
