package de.hpi.asg.breezetestgen.actors

import de.hpi.asg.breezetestgen.domain
import de.hpi.asg.breezetestgen.domain.Netlist
import de.hpi.asg.breezetestgen.domain.components.NetlistBehaviour
import de.hpi.asg.breezetestgen.testgeneration.InformationHub
import de.hpi.asg.breezetestgen.testing.TestEvent

class NetlistActor(id: Netlist.Id, netlist: NetlistBehaviour, infoHub: InformationHub) extends HandshakeActor {


  override protected def handleSignal(nl: domain.Netlist.Id, ds: domain.Signal, testEvent: TestEvent) = {
    val isInternal = nl == id
    val reaction = if (isInternal) {
      netlist.handleInternalSignal(ds, testEvent)
    } else {
      netlist.handleExternalSignal(ds, testEvent)
    }

  }
}
