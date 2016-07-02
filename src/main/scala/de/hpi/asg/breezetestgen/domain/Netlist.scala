package de.hpi.asg.breezetestgen.domain

import de.hpi.asg.breezetestgen.domain.components.brzcomponents.Loop
import de.hpi.asg.breezetestgen.domain.components.{BrzComponent, HandshakeComponent}

object Netlist {
  type Id = List[Int]
  val TopLevelId: Id = -1 :: Nil

  type State = HandshakeComponent.State[Null, Map[HandshakeComponent.Id, HandshakeComponent.State[_, _]]]
  object State {
    def apply(componentStates: Map[HandshakeComponent.Id, HandshakeComponent.State[_, _]]): State =
      HandshakeComponent.State(null, componentStates)
  }
}

/** data container for a whole Netlist */
case class Netlist(id: Netlist.Id,
                   name: String,
                   ports: Map[Port.Id, Port],
                   channels: Map[Channel.Id, Channel[Channel.Endpoint]],
                  //TODO: component should map to HandshakeComponent, not BrzComponent
                   components: Map[HandshakeComponent.Id, BrzComponent]) {

  /** returns all ports which are Active (as seen from the netlist itself) */
  def activePorts: Set[Port] = ports.values.collect{case p: Port if p.sense == Port.Active => p}.toSet
  /** returns all ports which are Passive (as seen from the netlist itself) */
  def passivePorts: Set[Port] = ports.values.collect{case p: Port if p.sense == Port.Passive => p}.toSet

  /** return all ids of loop components within this netlist */
  def loopIds: Set[HandshakeComponent.Id] = {
    components.collect {
      case (i, _: Loop) => i
    }.toSet
    // union subNetlists.map(_.loopIds)
  }
  /* //sadly, the general version won't work because of type erasure
  def componentTypeIds[C <: BrzComponent]: Set[List[HandshakeComponent.Id]] = {
    components.collect {
      case (i, _: C) => i
    }.toSet
    // union subNetlists.map(_.loopIds)
  }*/
}
