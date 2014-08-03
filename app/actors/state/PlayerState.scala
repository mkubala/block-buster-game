package actors.state

import akka.actor.Cancellable

case class PlayerState(name: String,
                       ticksScheduler: Cancellable,
                       block: Block,
                       board: Board) {

  //  def withNewBlock(newBLock: Block): PlayerState = copy(block = newBLock)

  def moveBlockDown: (PlayerState, Option[Block]) =
    if (block.isEmbed(board)) {
      val newBlock = Block.random
      (copy(board = board.embed(block), block = newBlock), Some(newBlock))
    } else (copy(block = block.oneRowDown), None)

}
