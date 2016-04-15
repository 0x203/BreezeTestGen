package de.hpi.asg.breezetestgen.actors

import akka.actor.ActorRef
import akka.pattern.{ask, pipe}
import akka.util.Timeout

import concurrent.duration._
import concurrent.Future
import de.hpi.asg.breezetestgen.actors.HandshakeActor.{GetState, MyState, Signal}
import de.hpi.asg.breezetestgen.domain
import de.hpi.asg.breezetestgen.domain.components.BrzComponentBehaviour.{DecisionRequired, NormalFlowReaction}
import de.hpi.asg.breezetestgen.domain.components.{BrzComponentBehaviour, HandshakeComponent}
import de.hpi.asg.breezetestgen.domain.{Netlist, SignalFromActive, SignalFromPassive}
import de.hpi.asg.breezetestgen.testing.TestEvent



class ComponentActor(netlistId: Netlist.Id,
                     componentId: HandshakeComponent.Id,
                     component: BrzComponentBehaviour[_, _],
                     infoHub: Option[ActorRef]) extends HandshakeActor {
  import ComponentActor._
  import context.dispatcher
  implicit val askTimeout = Timeout(5 seconds)

  receiue{
    case GetState => sender() ! MyState(netlistId, componentId, component.state)
    case Decision(newState, domainSignals, testEvent) =>
      component.state = newState
      for(ds <- domainSignals)
        receiverOf(ds) ! Signal(netlistId, ds, testEvent)
  }

  override protected def handleSignal(nlId: Netlist.Id, ds: domain.Signal, testEvent: TestEvent) = {
    require(netlistId == nlId)

    component.handleSignal(ds, testEvent) match {
      case nf: NormalFlowReaction => handleNormalFlow(testEvent, nf)
      case dr: DecisionRequired => handleDecisionRequired(testEvent, dr)
    }
  }

  private def handleDecisionRequired(testEvent: TestEvent, dr: DecisionRequired) = {
    infoHub match {
      case Some(hub) => hub ! dr
      case None =>
          error("No InformationHub given, but DecisionRequired!")
          context.stop(self)
    }
  }

  private def handleNormalFlow(testEvent: TestEvent, nf: NormalFlowReaction) = {
    val newTestEventF: Future[TestEvent] = infoHub match {
      case Some(hub) => (hub ? nf).mapTo[TestEvent]
      case None => Future.successful(testEvent)
    }

    for (signal <- nf.signals) {
      newTestEventF.map(Signal(netlistId, signal, _)) pipeTo receiverOf(signal)
    }
  }

  private def receiverOf(signal: domain.Signal): ActorRef = signal match {
    case _:SignalFromActive => channels(signal.channelId).passive
    case _:SignalFromPassive => channels(signal.channelId).active
  }

}

object ComponentActor {
  case class Decision(newState: HandshakeComponent.State[_, _], domainSignals: Set[domain.Signal], testEvent: TestEvent)
}
