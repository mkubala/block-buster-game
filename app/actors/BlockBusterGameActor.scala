package actors

import actors.messages.ConnectionMessages._
import actors.messages.GameMessages._
import actors.state.{PlayerState, GameState}
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import play.api.Play.current
import play.api.libs.concurrent.Akka
import play.api.libs.iteratee._
import play.api.libs.json._
import play.api.mvc.Result

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Random

object BlockBusterGameActor {

  val tickInterval = 500 milliseconds
  val boardHeight = 20
  
  lazy val defaultGame = Akka.system.actorOf(Props[BlockBusterGameActor], "defaultGame")

  implicit val defaultTimeout = Timeout(5 seconds)

  def join(username: String): Future[Either[Result, (Iteratee[JsValue, _], Enumerator[JsValue])]] = {
    (defaultGame ? Join(username)) map {
      case Connected(enumerator) =>
        val iteratee = Iteratee.foreach[JsValue] { event =>
          defaultGame ! PlayerEvent(event)
        } map { _ =>
          defaultGame ! Disconnected(username)
        }
        Right((iteratee, enumerator))
      case ConnectionError(cause) =>
        val iteratee = Done[JsValue, Unit]((), Input.EOF)
        val enumerator = Enumerator[JsValue](error(cause)).andThen(Enumerator.enumInput(Input.EOF))
        Right((iteratee, enumerator))
    }
  }

  def error(cause: String): JsObject = JsObject(Seq(
    ("kind" -> JsString("error")),
    ("payload" -> JsObject(Seq(
      ("cause" -> JsString(cause)))
    ))
  ))

}

class BlockBusterGameActor extends Actor with ActorLogging {

  val (inputEnumerator, outputChannel) = Concurrent.broadcast[JsValue]

  def waitingForPlayers(players: Set[String]): Receive = {
    case Join(username) =>
      log.debug(s"$username joined")
      sender() ! Connected(inputEnumerator)
      context.become(playingGame(setUpNewGame(players + username)))
    case Disconnected(username) => context.become(waitingForPlayers(players - username))
    case _ => ()
  }

  def playingGame(gameState: GameState): Receive = {
    case Tick(player) => self ! Move(player, MoveDown)
    case Disconnected(playerName) =>
      gameState.playerStates.get(playerName) foreach {
        _.ticksScheduler.cancel()
      }
      log.debug(s"Player $playerName has left the game.")
      if (gameState.playerStates.size == 1) {
        context.become(waitingForPlayers(Set.empty[String]))
      } else {
        context.become(playingGame(gameState.without(playerName)))
      }
    case Move(player, direction) =>
      if (direction == MoveDown) performMoveDown(gameState, Move(player, MoveDown))
    case PlayerEvent(eventJson) =>
      (eventJson \ "kind").asOpt[String] match {
        case Some(Move.kind) =>
          Json.fromJson[Move](eventJson) foreach { move =>
            performMoveDown(gameState, move)
          }
        case _ => log.error(s"UNKNOWN MESSAGE TYPE: $eventJson")
      }
    case _ => ()
  }

  def performMoveDown(gameState: GameState, move: Move): Unit = {
    val Move(playerName, direction) = move
    val (newState, genNewBlock) = gameState.moveBlockDown(playerName)
    log.debug(s"MOVE: $playerName, $direction, actual state = ${newState}")
    if (genNewBlock) {
      outputChannel.push(Json.toJson(BlockEmbedded(playerName, Random.nextInt(7))))
    } else {
      outputChannel.push(Json.toJson(move))
    }
    context.become(playingGame(newState))
  }

  def receive = waitingForPlayers(Set.empty[String])

  def setUpNewGame(players: Set[String]): GameState = {
    val playerStates: Map[String, PlayerState] = players.map { playerName =>
      val cancellable: Cancellable = Akka.system.scheduler.schedule(0 milliseconds, BlockBusterGameActor.tickInterval) {
        self ! Tick(playerName)
      }
      (playerName, PlayerState(playerName, cancellable, BlockBusterGameActor.boardHeight))
    }.toMap
    GameState(playerStates, BlockBusterGameActor.boardHeight)
  }

}









