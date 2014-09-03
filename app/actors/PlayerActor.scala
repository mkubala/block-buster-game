package actors

import actors.messages.GameMessages.{BlockEmbedded, BlockMoved, Move, MoveDown}
import actors.state.{Block, PlayerState}
import akka.actor.{Actor, ActorLogging, Props}

object PlayerActor {

  def named(playerName: String): Props = Props(classOf[PlayerActor], playerName)

  case object Tick

  case class Start(block: Block)

}

class PlayerActor(playerName: String) extends Actor with ActorLogging {

  override def receive = waiting

  override def preStart() {
    implicit val ec = context.dispatcher
    context.system.scheduler.scheduleOnce(PlayerState.defaultTickInterval, self, PlayerActor.Tick)
  }

  override def postRestart(reason: Throwable): Unit = {}

  def waiting: Receive = {
    case PlayerActor.Start(block) =>
      log.debug(s"player $playerName have received Start message")
      context.become(playingGame(PlayerState.withBlock(block)))
  }

  def playingGame(state: PlayerState): Receive = {
    case PlayerActor.Tick =>
      self ! Move(playerName, MoveDown)
      implicit val ec = context.dispatcher
      context.system.scheduler.scheduleOnce(state.tickInterval, self, PlayerActor.Tick)
    case Move(_, MoveDown) => {
      val (newState, maybeNewBlock) = state.moveBlockDown
      context.parent ! (maybeNewBlock map {
        newBlock =>
          BlockEmbedded(playerName, newBlock)
      } getOrElse {
        BlockMoved(playerName, MoveDown)
      })
      context.become(playingGame(newState))
    }
  }


}