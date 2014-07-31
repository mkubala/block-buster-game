package actors.state

import actors.messages.GameMessages.BlockEmbedded
import akka.actor.Cancellable

case class PlayerState(name: String,
                       ticksScheduler: Cancellable,
                       blockY: Int = 0,
                       board: PlayerState.Board = Array.fill(PlayerState.boardHeight) {
                         Array.fill(PlayerState.boardWidth)(false)
                       }) {

  def withNewBlock(shape: BlockEmbedded.BlockShape): PlayerState = copy(blockY = 0)

  def moveBlockDown: (PlayerState, Boolean) =
    if (blockY + 1 < PlayerState.boardHeight && board(blockY + 1).forall(!_))
      (copy(blockY = blockY + 1), false)
    else
      (copy(blockY = 0, board = boardWithBlock(PlayerState.boardWidth / 2, blockY)), true)

  def boardWithBlock(x: Int, y: Int): PlayerState.Board =
    board.updated(blockY, board(y).updated(x, true))

}

object PlayerState {
  type Board = Array[Array[Boolean]]

  val boardWidth = 10
  val boardHeight = 20
}
