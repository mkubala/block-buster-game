package actors.state

case class GameState(playerStates: Map[String, PlayerState]) {

  def without(playerName: String): GameState = copy(playerStates = playerStates - playerName)

  def withUpdatedPlayerState(playerState: PlayerState) =
    copy(playerStates = playerStates + (playerState.name -> playerState))

  def moveBlockDown(playerName: String): (GameState, Option[Block]) = {
    playerStates.get(playerName) match {
      case Some(playerState) =>
        val (newPlayerState, maybeNewBlock) = playerState.moveBlockDown
        (withUpdatedPlayerState(newPlayerState), maybeNewBlock)
      case None => (this, None)
    }
  }
}
