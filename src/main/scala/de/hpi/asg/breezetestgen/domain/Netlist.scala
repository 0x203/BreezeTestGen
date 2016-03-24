package de.hpi.asg.breezetestgen.domain

import de.hpi.asg.breezetestgen.domain.components.BrzComponent

object Netlist {
  type Id = Int
}

/** data container for a whole Netlist */
case class Netlist(id: Netlist.Id,
                   ports: Set[Port],
                   channels: Set[Channel[Channel.Endpoint]],
                   components: Set[BrzComponent]) {
  /** returns all ports which are Active (as seen from the netlist itself (just POC for now*/
  def activePorts: Set[Port] = ports.collect{case p: Port if p.sense == Port.Active => p}
}
