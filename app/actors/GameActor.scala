package actors

import actors.messages.ConnectionMessages._
import actors.messages.GameMessages._
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
import actors.messages.Message

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
      gameState.players.values foreach (_ ! PlayerActor.Start(Block.random))
      context.become(playingGame(gameState))
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
//      gameState.players.get(playerName) foreach (_ ! PoisonPill)
      log.debug(s"Player $playerName has left the game.")
      if (gameState.players.size == 1) {
        context.become(waitingForPlayers(Set.empty[String]))
      } else {
        context.become(playingGame(gameState.withoutPlayer(playerName)))
      }
    case BlockMoved(playerName, direction) =>
      sendBroadcast(Json.toJson(Move(playerName, direction)))
    case e: BlockEmbedded =>
      sendBroadcast(Json.toJson(e))
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

object PlayerActor {

  def named(playerName: String): Props = Props(classOf[PlayerActor], playerName)

  case object Tick

  case class Start(block: Block)

//  case class PlayerStarted(blockShape: Block.Shape)

}

class PlayerActor(playerName: String) extends Actor with ActorLogging {

  override def receive = waiting


  override def preStart(): Unit = {
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