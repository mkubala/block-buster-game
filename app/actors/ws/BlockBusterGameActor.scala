package actors.ws

import actors.ws.BlockEmbedded.BlockShape
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

case class PlayerState(name: String, ticksScheduler: Cancellable, blockPos: Int)

case class Join(username: String)

case class Connected(enumerator: Enumerator[JsValue])

case class ConnectionError(cause: String)


trait Message {
  def kind: String

  def playerName: String
}

object Message {
  def json[T](kind: String, player: String, payload: JsValue): JsValue =
    JsObject(Seq(
      ("kind" -> JsString(kind)),
      ("player" -> JsString(player)),
      ("payload" -> payload)
    ))
}

case class Tick(playerName: String) extends Message {
  val kind = "tick"
}

object Tick {
  implicit val jsonWrites: Writes[Tick] = new Writes[Tick] {
    override def writes(tick: Tick): JsValue =
      Message.json("tick", tick.playerName, JsObject(Seq.empty[(String, JsValue)]))
  }
}

sealed class MoveDirection(val encodedVal: Int)

case object MoveLeft extends MoveDirection(-1)

case object MoveDown extends MoveDirection(0)

case object MoveRight extends MoveDirection(1)

object MoveDirection {
  def apply(encodedValue: Int): MoveDirection =
    if (encodedValue == -1) {
      MoveLeft
    } else if (encodedValue == 1) {
      MoveRight
    } else {
      MoveDown
    }
}

case class Move(playerName: String, direction: MoveDirection) extends Message {
  val kind = Move.kind
}

object Move {
  val kind = "move"

  implicit val jsonFormat: Format[Move] = new Format[Move] {

    override def writes(move: Move): JsValue = Message.json(Move.kind, move.playerName, JsObject(Seq(
      ("direction" -> JsNumber(move.direction.encodedVal))
    )))

    override def reads(json: JsValue): JsResult[Move] = JsSuccess {
      val player = (json \ "player").as[String]
      val direction = MoveDirection((json \ "payload" \ "direction").as[Int])
      Move(player, direction)
    }
  }
}

case class PlayerEvent(event: JsValue)

case class Disconnected(playerName: String)

case class BlockEmbedded(playerName: String, blockShape: BlockShape) extends Message {
  val kind = BlockEmbedded.kind
}

object BlockEmbedded {
  type BlockShape = Int
  val kind = "embedded"

  implicit val jsonFormat: Format[BlockEmbedded] = new Format[BlockEmbedded] {
    override def writes(be: BlockEmbedded): JsValue = {
      val payload = JsObject(Seq(
        ("newBlock" -> JsObject(Seq(
          ("shape" -> JsNumber(be.blockShape))
        )))
      ))
      Message.json(kind, be.playerName, payload)
    }

    override def reads(json: JsValue): JsResult[BlockEmbedded] =
      (for {
        kind <- (json \ "kind").asOpt[String]
        if (kind == BlockEmbedded.kind)
        player <- (json \ "player").asOpt[String]
        shape <- (json \ "payload" \ "newBlock" \ "shape").asOpt[BlockShape]
      } yield BlockEmbedded(player, shape)).map(JsSuccess(_)).getOrElse(JsError())
  }

}

