package de.hpi.asg.breezetestgen.actors

import akka.actor.ActorRef
import de.hpi.asg.breezetestgen.actors.HandshakeActor._
import de.hpi.asg.breezetestgen.domain
import de.hpi.asg.breezetestgen.domain.{SignalFromPassive, SignalFromActive, ComponentBehaviour}
import de.hpi.asg.breezetestgen.testing.TestEvent

class ComponentActor(component: ComponentBehaviour[_, _],
                     infoHub: ActorRef) extends HandshakeActor {
  when(Initialized) {
    case Event(Signal(domainSignal, testEvent), _) =>
      handleSignal(domainSignal.asInstanceOf[domain.Signal], testEvent)
      stay()
  }

  private def handleSignal(ds: domain.Signal, testEvent: TestEvent) = {
    val reaction = component.handleSignal(ds, testEvent)
    reaction.signals.foreach{s => receiverOf(s) ! s}

    // TODO: implement wiser handling of this
    infoHub ! (reaction.testOp, reaction.constraintVariables)
  }

  private def receiverOf(signal: domain.Signal): ActorRef = signal match {
    case _:SignalFromActive => channels(signal.channelId).passive
    case _:SignalFromPassive => channels(signal.channelId).active
  }
}
