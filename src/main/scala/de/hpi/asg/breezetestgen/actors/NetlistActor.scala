package de.hpi.asg.breezetestgen.actors

import de.hpi.asg.breezetestgen.domain
import de.hpi.asg.breezetestgen.domain.components.Netlist
import de.hpi.asg.breezetestgen.testgeneration.InformationHub
import de.hpi.asg.breezetestgen.testing.TestEvent

class NetlistActor(netlist: Netlist, infoHub: InformationHub) extends HandshakeActor {


  override protected def handleSignal(ds: domain.Signal, testEvent: TestEvent) = {

  }
}
