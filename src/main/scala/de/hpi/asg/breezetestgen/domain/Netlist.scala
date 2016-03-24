package de.hpi.asg.breezetestgen.domain

import de.hpi.asg.breezetestgen.domain.components.{BrzComponent, HandshakeComponent}

object Netlist {
  type Id = Int
}

/** data container for a whole Netlist */
case class Netlist(id: Netlist.Id,
                   ports: Map[Port.Id, Port],
                   channels: Map[Channel.Id, Channel[Channel.Endpoint]],
                   components: Map[HandshakeComponent.Id, BrzComponent]) {
  /** returns all ports which are Active (as seen from the netlist itself (just POC for now*/
  def activePorts: Set[Port] = ports.values.collect{case p: Port if p.sense == Port.Active => p}.toSet
}
