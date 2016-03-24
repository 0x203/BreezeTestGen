package de.hpi.asg.breezetestgen.actors

import akka.actor.{ActorRef, FSM}
import de.hpi.asg.breezetestgen.domain
import de.hpi.asg.breezetestgen.testing.TestEvent
import domain.Channel

object HandshakeActor {
  type ChannelMap = Channel.Id => Channel[ActorRef]
  case class SetChannels(channels: ChannelMap)

  sealed trait States
  case object Uninitialized extends States
  case object Initialized extends States

  case class Signal(domainSignal: domain.Signal, testEvent: TestEvent)
}

abstract class HandshakeActor extends FSM[HandshakeActor.States, HandshakeActor.ChannelMap] {
  import HandshakeActor._

  startWith(Uninitialized, Map.empty)

  when(Uninitialized) {
    case Event(SetChannels(channels), _) =>
      goto(Initialized) using channels
  }

  when(Initialized) {
    case Event(Signal(domainSignal, testEvent), _) =>
      handleSignal(domainSignal.asInstanceOf[domain.Signal], testEvent)
      stay()
  }

  protected def channels: ChannelMap = stateData

  protected def handleSignal(ds: domain.Signal, testEvent: TestEvent)
}