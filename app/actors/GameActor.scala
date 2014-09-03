package actors

import actors.messages.ConnectionMessages._
import actors.messages.GameMessages._
import actors.messages.Message
import actors.state._
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

object GameActor {

  lazy val defaultGame = Akka.system.actorOf(Props[GameActor], "defaultGame")

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
    "kind" -> JsString("error"),
    "payload" -> JsObject(Seq(
      "cause" -> JsString(cause)
    ))
  ))


}

class GameActor extends Actor with ActorLogging {

  private val (inputEnumerator, outputChannel) = Concurrent.broadcast[JsValue]

  def waitingForPlayers(players: Set[String]): Receive = {
    case Join(username) =>
      log.debug(s"$username joined")
      sender() ! Connected(inputEnumerator)
      val gameState = setUpNewGame(players + username)
      val playerBlocks = gameState.players.keys map (pn => (pn -> Block.random))
      for {
        (playerName, block) <- playerBlocks
        playerRef <- gameState.players.get(playerName)
      } playerRef ! PlayerActor.Start(block)

      context.become(playingGame(gameState))
      sendBroadcast(GameStarted(playerBlocks.toList))
    case Disconnected(username) => context.become(waitingForPlayers(players - username))
    case _ => ()
  }

  def playingGame(gameState: GameState): Receive = receivePlayerEvent {
    case msg: Message => msg.playerName match {
      case Some(playerName) => gameState.players.get(playerName) foreach (_ ! msg)
      case None => self ! msg // TODO mkubala: consider additional logging
    }
  } orElse {
    case Disconnected(playerName) =>
      gameState.players.get(playerName) foreach (_ ! PoisonPill)
      log.debug(s"Player $playerName has left the game.")
      if (gameState.players.size == 1) {
        context.become(waitingForPlayers(Set.empty[String]))
      } else {
        context.become(playingGame(gameState.withoutPlayer(playerName)))
      }
    case BlockMoved(playerName, direction) => sendBroadcast(Move(playerName, direction))
    case e: BlockEmbedded => sendBroadcast(e)
  }

  def receivePlayerEvent(playerEventReceive: (Message => Unit)): Receive = {
    case event: PlayerEvent => {
      val payload: JsValue = event.payload
      val parsedMessage: Option[Message] = (payload \ "kind").asOpt[String] flatMap {
        case Move.kind => Json.fromJson[Move](payload).asOpt
      }
      playerEventReceive(parsedMessage.getOrElse(UnsupportedMessage))
    }
  }

  def sendBroadcast[T <: Message](msg: T)(implicit w: Writes[T]) {
    sendBroadcast(Message.json(msg.kind, msg.playerName, w.writes(msg)))
  }

  def sendBroadcast(message: JsValue) {
    outputChannel.push(message)
  }

  def receive = waitingForPlayers(Set.empty[String])

  def setUpNewGame(players: Set[String]): GameState = GameState {
    players.map { name =>
      name -> context.actorOf(PlayerActor.named(name), name)
    }(collection.breakOut)
  }

}
