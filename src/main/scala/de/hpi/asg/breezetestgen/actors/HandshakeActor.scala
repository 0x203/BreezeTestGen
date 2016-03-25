package de.hpi.asg.breezetestgen.actors

import akka.actor.{ActorRef, FSM}
import de.hpi.asg.breezetestgen.domain
import de.hpi.asg.breezetestgen.testing.TestEvent
import domain.{Channel, Netlist, SignalFromActive, SignalFromPassive}

object HandshakeActor {
  type ChannelMap = Channel.Id => Channel[ActorRef]
  case class SetChannels(channels: ChannelMap)

  sealed trait States
  case object Uninitialized extends States
  case object Initialized extends States

  case class Signal(netlist: Netlist.Id, domainSignal: domain.Signal, testEvent: TestEvent)
}

abstract class HandshakeActor extends FSM[HandshakeActor.States, HandshakeActor.ChannelMap] {
  import HandshakeActor._

  startWith(Uninitialized, Map.empty)

  when(Uninitialized) {
    case Event(SetChannels(channels), _) =>
      goto(Initialized) using channels
  }

  when(Initialized) {
    case Event(Signal(netlist, domainSignal, testEvent), _) =>
      handleSignal(netlist, domainSignal, testEvent)
      stay()
  }

  protected def receiverOf(signal: domain.Signal): ActorRef = signal match {
    case _:SignalFromActive => channels(signal.channelId).passive
    case _:SignalFromPassive => channels(signal.channelId).active
  }

  /** record custom receive function for subclasses, because im an FSM */
  protected def receiue(f: PartialFunction[Any, _]): Unit = {
    whenUnhandled{
      case Event(a, _) =>
        f(a)
        stay()
    }
  }

  protected def channels: ChannelMap = stateData

  protected def handleSignal(netlist: Netlist.Id, ds: domain.Signal, testEvent: TestEvent)
}