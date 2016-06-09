package de.hpi.asg.breezetestgen.actors

import akka.actor.{ActorRef, FSM}
import de.hpi.asg.breezetestgen.domain.components.HandshakeComponent
import de.hpi.asg.breezetestgen.{Loggable, domain}
import de.hpi.asg.breezetestgen.testing.TestEvent
import domain.{Channel, Netlist}

object HandshakeActor {
  type ChannelMap = Channel.Id => Channel[ActorRef]
  case class SetChannels(channels: ChannelMap)

  sealed trait States
  case object Uninitialized extends States
  case object Initialized extends States

  case class Signal(idChain: List[Netlist.Id], domainSignal: domain.Signal, testEvent: TestEvent)
  case class DecisionRequired(idChain: List[Netlist.Id],
                              componentId: HandshakeComponent.Id,
                              domainDR: domain.components.BrzComponentBehaviour.DecisionRequired)

  case object GetState
  case class MyState(idChain: List[Netlist.Id], id: HandshakeComponent.Id, state: HandshakeComponent.State[_, _])
}

abstract class HandshakeActor extends FSM[HandshakeActor.States, HandshakeActor.ChannelMap] with Loggable {
  import HandshakeActor._

  startWith(Uninitialized, Map.empty)

  when(Uninitialized) {
    case Event(SetChannels(channels), _) =>
      trace("received ChannelMaps")
      goto(Initialized) using channels
  }

  when(Initialized) {
    case Event(Signal(idChain, domainSignal, testEvent), _) =>
      trace("received something in Initialized state")
      handleSignal(idChain, domainSignal, testEvent)
      stay()
  }

  /** record custom receive function for subclasses, because im an FSM */
  protected def receiue(f: PartialFunction[Any, _]): Unit = {
    whenUnhandled{
      case Event(a, _) =>
        if (f.isDefinedAt(a))
          f(a)
        stay()
    }
  }

  protected def channels: ChannelMap = stateData

  protected def handleSignal(idChain: List[Netlist.Id], ds: domain.Signal, testEvent: TestEvent)
}