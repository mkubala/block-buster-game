package actors.state

import akka.actor.Cancellable

case class PlayerState(name: String, ticksScheduler: Cancellable, blockPos: Int)
