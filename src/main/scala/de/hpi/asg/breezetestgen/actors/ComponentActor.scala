package de.hpi.asg.breezetestgen.actors

import akka.actor.ActorRef
import akka.pattern.{ask, pipe}
import akka.util.Timeout

import concurrent.duration._
import concurrent.Future
import de.hpi.asg.breezetestgen.actors.HandshakeActor._
import de.hpi.asg.breezetestgen.domain
import de.hpi.asg.breezetestgen.domain.components.{BrzComponentBehaviour, HandshakeComponent}
import de.hpi.asg.breezetestgen.domain.{SignalFromActive, SignalFromPassive}
import de.hpi.asg.breezetestgen.testing.TestEvent



class ComponentActor(runId: Int,
                     componentId: HandshakeComponent.Id,
                     component: BrzComponentBehaviour[_, _],
                     infoHub: Option[ActorRef]) extends HandshakeActor {
  import context.dispatcher
  implicit val askTimeout = Timeout(5 seconds)

  receiue{
    case GetState => sender() ! MyState(runId, componentId, component.state)

    case Decision(`componentId`, newState, domainSignals, testEvent) =>
      info(s"$componentId: Got Decision: $newState; $domainSignals; $testEvent")
      component.state = newState
      for(ds <- domainSignals)
        receiverOf(ds) ! Signal(runId, componentId, ds, testEvent)
  }

  override protected def handleSignal(senderId: HandshakeComponent.Id, ds: domain.Signal, testEvent: TestEvent) = {
    component.handleSignal(ds, testEvent) match {
      case nf: BrzComponentBehaviour.NormalFlowReaction => handleNormalFlow(testEvent, nf)
      case dr: BrzComponentBehaviour.DecisionRequired => handleDecisionRequired(testEvent, dr)
    }
  }

  private def handleDecisionRequired(testEvent: TestEvent, ddr: BrzComponentBehaviour.DecisionRequired) = {
    infoHub match {
      case Some(hub) => hub ! DecisionRequired(runId, componentId, ddr)
      case None =>
          error(s"$componentId: No InformationHub given, but DecisionRequired!")
          context.stop(self)
    }
  }

  private def handleNormalFlow(testEvent: TestEvent, nf: BrzComponentBehaviour.NormalFlowReaction) = {
    val newTestEventF: Future[TestEvent] = infoHub match {
      case Some(hub) => (hub ? NormalFlowReaction(runId, componentId, nf)).mapTo[TestEvent]
      case None => Future.successful(testEvent)
    }

    for (signal <- nf.signals) {
      newTestEventF.map(Signal(runId, componentId, signal, _)) pipeTo receiverOf(signal)
    }
  }

  private def receiverOf(signal: domain.Signal): ActorRef = signal match {
    case _:SignalFromActive => channels(signal.channelId).passive
    case _:SignalFromPassive => channels(signal.channelId).active
  }

}
