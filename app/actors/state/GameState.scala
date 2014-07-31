package actors.state

case class GameState(playerStates: Map[String, PlayerState]) {

  def without(playerName: String): GameState = copy(playerStates = playerStates - playerName)

  def withUpdatedPlayerState(playerState: PlayerState) =
    copy(playerStates = playerStates + (playerState.name -> playerState))

  def moveBlockDown(playerName: String): (GameState, Boolean) = {
    playerStates.get(playerName) match {
      case Some(playerState) =>
        val (newPlayerState, blockHasBeenEmbedded) = playerState.moveBlockDown
        (withUpdatedPlayerState(newPlayerState), blockHasBeenEmbedded)
      case None => (this, false)
    }
  }
}
