package de.hpi.asg.breezetestgen.domain.components

import ComponentBehaviour.Reaction
import de.hpi.asg.breezetestgen.domain._
import de.hpi.asg.breezetestgen.testing.TestEvent

/** represents a sub-netlist, i.e. a complete netlist wrapped into component which can be used in another netlist */
class Netlist(val id: HandshakeComponent.Id, netlist: de.hpi.asg.breezetestgen.domain.Netlist)
  extends HandshakeComponent {
  import NetlistBehaviour._

  type Behaviour = NetlistBehaviour
  type C = ControlState
  type D = PortStates

  def behaviour(state: Option[ComponentState[C, D]]): Behaviour =
    new NetlistBehaviour(state getOrElse NetlistBehaviour.freshState)

  object NetlistBehaviour {
    type ControlState = Null
    type PortStates = Map[Port.Id, Port.State]

    val freshState: ComponentState[ControlState, PortStates] =
      ComponentState(null, netlist.ports.map{_.id -> Port.Idle}.toMap)
  }

  class NetlistBehaviour(initState: ComponentState[C, D]) {
    def handleInternalSignal(port: Port, s: Signal, te: TestEvent): Reaction = {
      var reaction = Reaction.empty
      reaction
    }

    def handleExternalSignal(port: Port, s: Signal, te: TestEvent): Reaction = {
      var reaction = Reaction.empty
      reaction
    }
  }
}
