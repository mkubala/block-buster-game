package actors.state

case class GameState(playerStates: Map[String, PlayerState], boardHeight: Int = 20) {

   def without(playerName: String): GameState = copy(playerStates = playerStates - playerName)

   def moveBlockDown(playerName: String): (GameState, Boolean) = {
     playerStates.get(playerName) match {
       case Some(playerState) =>
         if (playerState.blockPos == 0) {
           val newState: GameState = copy(playerStates + (playerName -> playerState.copy(blockPos = boardHeight)))
           (newState, true)
         } else {
           val newState: GameState = copy(playerStates + (playerName -> playerState.copy(blockPos = playerState.blockPos - 1)))
           (newState, false)
         }
       case None => (this, false)
     }
   }
 }
