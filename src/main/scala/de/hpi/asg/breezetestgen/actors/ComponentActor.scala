package de.hpi.asg.breezetestgen.actors

import akka.actor.ActorRef
import de.hpi.asg.breezetestgen.actors.HandshakeActor.Signal
import de.hpi.asg.breezetestgen.domain
import de.hpi.asg.breezetestgen.domain.components.BrzComponentBehaviour
import de.hpi.asg.breezetestgen.domain.Netlist
import de.hpi.asg.breezetestgen.testing.TestEvent

class ComponentActor(component: BrzComponentBehaviour[_, _],
                     infoHub: ActorRef) extends HandshakeActor {

  override protected def handleSignal(netlist: Netlist.Id, ds: domain.Signal, testEvent: TestEvent) = {
    val reaction = component.handleSignal(ds, testEvent)

    //TODO: handle decision making!

    // handle TestOp
    val newTestEvent = reaction.testOp match {
      case None =>  testEvent
      case Some(op) =>
        //TODO: perform testOp and get new TestEvent
        testEvent
    }

    // send out constraints
    infoHub ! reaction.constraintVariables

    // send out new signals
    sendOutSignals(netlist, newTestEvent, reaction.signals)
  }

  private def sendOutSignals(netlist: Netlist.Id, testEvent: TestEvent, ds: Set[domain.Signal]) = {
    ds.foreach{s => receiverOf(s) ! Signal(netlist, s, testEvent)}
  }
}
