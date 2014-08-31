package actors.state

import akka.actor.ActorRef

case class GameState(players: Map[String, ActorRef]) {

  def withoutPlayer(playerName: String): GameState = copy(players = players - playerName)

  def withPlayer(playerName: String, player: ActorRef) =
    copy(players = players + (playerName -> player))

}
