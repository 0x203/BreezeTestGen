package de.hpi.asg.breezetestgen.actors

import akka.actor.ActorRef
import akka.pattern.{ask, pipe}

import concurrent.Future
import de.hpi.asg.breezetestgen.actors.HandshakeActor.{GetState, MyState, Signal}
import de.hpi.asg.breezetestgen.domain
import de.hpi.asg.breezetestgen.domain.components.BrzComponentBehaviour.{DecisionRequired, NormalFlowReaction, Reaction}
import de.hpi.asg.breezetestgen.domain.components.{BrzComponentBehaviour, HandshakeComponent}
import de.hpi.asg.breezetestgen.domain.{Netlist, SignalFromActive, SignalFromPassive}
import de.hpi.asg.breezetestgen.testing.TestEvent

class ComponentActor(netlistId: Netlist.Id,
                     componentId: HandshakeComponent.Id,
                     component: BrzComponentBehaviour[_, _],
                     infoHub: Option[ActorRef]) extends HandshakeActor {

  receiue{
    case GetState => sender() ! MyState(netlistId, componentId, component.state)
  }

  override protected def handleSignal(nlId: Netlist.Id, ds: domain.Signal, testEvent: TestEvent) = {
    require(netlistId == nlId)

    component.handleSignal(ds, testEvent) match {
      case nf: NormalFlowReaction => handleNormalFlow(nlId, testEvent, nf)
      case dr: DecisionRequired => handleDecisionRequired(nlId, testEvent, dr)
    }
  }

  private def handleDecisionRequired(nlId: Netlist.Id, testEvent: TestEvent, dr: DecisionRequired) = {
    infoHub match {
      case Some(hub) =>
        //TODO: let others decide and set state accordingly
        //val decisionF = hub ? dr
      case None =>
          error("No InformationHub given, but DecisionRequired!")
          context.stop(self)
    }
  }

  private def handleNormalFlow(nlId: Netlist.Id, testEvent: TestEvent, nf: NormalFlowReaction) = {
    import context.dispatcher
    import concurrent.duration._
    import akka.util.Timeout

    implicit val askTimeout = Timeout(5 seconds)

    val newTestEventF: Future[TestEvent] = infoHub
      .map(_ ? nf)  // ask InfoHub
      .map(_.mapTo[TestEvent]) getOrElse  // transform answer to right type
      Future.successful(testEvent)  //fallback if no infoHub is defined

    for (signal <- nf.signals) {
      newTestEventF.map(Signal(nlId, signal, _)) pipeTo receiverOf(signal)
    }
  }

  private def sendOutSignals(netlist: Netlist.Id, testEvent: TestEvent, ds: Set[domain.Signal]) = {
    ds.foreach{s => receiverOf(s) ! Signal(netlist, s, testEvent)}
  }

  private def receiverOf(signal: domain.Signal): ActorRef = signal match {
    case _:SignalFromActive => channels(signal.channelId).passive
    case _:SignalFromPassive => channels(signal.channelId).active
  }

}
