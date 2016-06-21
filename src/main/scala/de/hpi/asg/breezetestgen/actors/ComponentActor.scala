package de.hpi.asg.breezetestgen.actors

import akka.actor.ActorRef
import akka.pattern.{ask, pipe}
import akka.util.Timeout

import concurrent.duration._
import concurrent.Future
import de.hpi.asg.breezetestgen.actors.HandshakeActor.{DecisionRequired, GetState, MyState, Signal, NormalFlowReaction}
import de.hpi.asg.breezetestgen.domain
import de.hpi.asg.breezetestgen.domain.components.{BrzComponentBehaviour, HandshakeComponent}
import de.hpi.asg.breezetestgen.domain.{Netlist, SignalFromActive, SignalFromPassive}
import de.hpi.asg.breezetestgen.testing.TestEvent



class ComponentActor(idChain: List[Netlist.Id],
                     componentId: HandshakeComponent.Id,
                     component: BrzComponentBehaviour[_, _],
                     infoHub: Option[ActorRef]) extends HandshakeActor {
  import ComponentActor._
  import context.dispatcher
  implicit val askTimeout = Timeout(5 seconds)

  receiue{
    case GetState => sender() ! MyState(idChain, componentId, component.state)

    case Decision(_, `componentId`, newState, domainSignals, testEvent) =>
      info(s"$componentId: Got Decision: $newState; $domainSignals; $testEvent")
      component.state = newState
      for(ds <- domainSignals)
        receiverOf(ds) ! Signal(idChain, ds, testEvent)
  }

  override protected def handleSignal(senderIdChain: List[Netlist.Id], ds: domain.Signal, testEvent: TestEvent) = {
    require(idChain == senderIdChain)

    component.handleSignal(ds, testEvent) match {
      case nf: BrzComponentBehaviour.NormalFlowReaction => handleNormalFlow(testEvent, nf)
      case dr: BrzComponentBehaviour.DecisionRequired => handleDecisionRequired(testEvent, dr)
    }
  }

  private def handleDecisionRequired(testEvent: TestEvent, ddr: BrzComponentBehaviour.DecisionRequired) = {
    infoHub match {
      case Some(hub) => hub ! DecisionRequired(idChain, componentId, ddr)
      case None =>
          error(s"$componentId: No InformationHub given, but DecisionRequired!")
          context.stop(self)
    }
  }

  private def handleNormalFlow(testEvent: TestEvent, nf: BrzComponentBehaviour.NormalFlowReaction) = {
    val newTestEventF: Future[TestEvent] = infoHub match {
      case Some(hub) => (hub ? NormalFlowReaction(idChain, nf)).mapTo[TestEvent]
      case None => Future.successful(testEvent)
    }

    for (signal <- nf.signals) {
      newTestEventF.map(Signal(idChain, signal, _)) pipeTo receiverOf(signal)
    }
  }

  private def receiverOf(signal: domain.Signal): ActorRef = signal match {
    case _:SignalFromActive => channels(signal.channelId).passive
    case _:SignalFromPassive => channels(signal.channelId).active
  }

}

object ComponentActor {
  case class Decision(idChain: List[Netlist.Id],
                      componentId: HandshakeComponent.Id,
                      newState: HandshakeComponent.State[_, _],
                      domainSignals: Set[domain.Signal],
                      testEvent: TestEvent)
}
