package de.hpi.asg.breezetestgen.actors

import de.hpi.asg.breezetestgen.actors.HandshakeActor._
import de.hpi.asg.breezetestgen.domain

class NetlistActor extends HandshakeActor {

  when(Initialized) {
    case Event(Signal(domainSignal: domain.Signal, testEvent), _) =>
      //handleSignal(domainSignal.asInstanceOf[domain.Signal], testEvent)
      stay()
  }
}
