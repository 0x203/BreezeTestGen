package de.hpi.asg.breezetestgen.domain

import de.hpi.asg.breezetestgen.domain.components.{BrzComponent, HandshakeComponent}

object Netlist {
  type Id = Int

  private[this] var currentId: Id = 0
  def nextId: Id = {
    currentId += 1
    currentId
  }

  case class State(componentStates: Map[HandshakeComponent.Id, HandshakeComponent.State[_, _]])
}

/** data container for a whole Netlist */
case class Netlist(id: Netlist.Id,
                   ports: Map[Port.Id, Port],
                   channels: Map[Channel.Id, Channel[Channel.Endpoint]],
                  //TODO: component should map to HandshakeComponent, not BrzComponent
                   components: Map[HandshakeComponent.Id, BrzComponent]) {

  /** returns all ports which are Active (as seen from the netlist itself) */
  def activePorts: Set[Port] = ports.values.collect{case p: Port if p.sense == Port.Active => p}.toSet
  /** returns all ports which are Passive (as seen from the netlist itself) */
  def passivePorts: Set[Port] = ports.values.collect{case p: Port if p.sense == Port.Passive => p}.toSet
}
