package de.hpi.asg.breezetestgen.domain

import de.hpi.asg.breezetestgen.domain.components.brzcomponents.Loop
import de.hpi.asg.breezetestgen.domain.components.{BrzComponent, HandshakeComponent}

object Netlist {
  type Id = Int

  private[this] var currentId: Id = 0
  def nextId: Id = {
    currentId += 1
    currentId
  }

  type State = HandshakeComponent.State[Null, Map[HandshakeComponent.Id, HandshakeComponent.State[_, _]]]
  object State {
    def apply(componentStates: Map[HandshakeComponent.Id, HandshakeComponent.State[_, _]]): State =
      HandshakeComponent.State(null, componentStates)
  }
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

  /** return all ids of loop components within this netlist */
  def loopIds: Set[List[HandshakeComponent.Id]] = {
    components.collect {
      case (i, l: Loop) => id ::  i :: Nil
      //case (i, snl: Netlist) => i :: snl.loopIds  // for hierarchical netlists
    }.toSet
  }
  /* //sadly, the general version won't work because of type erasure
  def componentTypeIds[C <: BrzComponent]: Set[List[HandshakeComponent.Id]] = {
    components.collect{
      case (i, c) if c.isInstanceOf[C] => i :: Nil
      //case (i, snl: Netlist) if snl.isInstanceOf[Netlist] => i :: snl.componentTypeIds[C]  // for hierarchical netlists
    }.toSet
  }*/
}
