package de.hpi.asg.breezetestgen.domain.components

import de.hpi.asg.breezetestgen.domain
import de.hpi.asg.breezetestgen.domain._
import domain.components.Component.Reaction
import de.hpi.asg.breezetestgen.testing.TestEvent

/** represents a sub-netlist, i.e. a complete netlist wrapped into component which can be used in another netlist */
class Netlist(val id: HandshakeComponent.Id,
              portsOutsideIn: Map[Channel.Id, Channel.Id],
              netlist: de.hpi.asg.breezetestgen.domain.Netlist)
  extends HandshakeComponent {
  import NetlistBehaviour._

  type Behaviour = NetlistBehaviour
  type C = ControlState
  type D = PortStates

  val portsInsideOut = portsOutsideIn.map(_.swap)

  def behaviour(state: Option[Component.State[C, D]]): Behaviour =
    new NetlistBehaviour(state getOrElse NetlistBehaviour.freshState)

  object NetlistBehaviour {
    type ControlState = Null
    type PortStates = Map[Port.Id, Port.State]

    val freshState: Component.State[ControlState, PortStates] =
      Component.State(null, netlist.ports.map{_._1 -> Port.Idle})
  }

  class NetlistBehaviour(initState: Component.State[C, D]) extends domain.components.NetlistBehaviour {
    // TODO: evaluate if this could be done completly in NetlistActor with much less overhead/code
    var state: PortStates = initState.dataState

    def handleInternalSignal(s: Signal, te: TestEvent): Reaction = {
      val outerChannelId = portsInsideOut(s.channelId)
      val new_s = Signal.changeId(s, outerChannelId)
      // TODO: change state
      // TODO: evaluate if TestEvent and/or ConstraintVariable should be set
      Reaction(Set(new_s), None, Set.empty)
    }

    def handleExternalSignal(s: Signal, te: TestEvent): Reaction = {
      val innerChannelId = portsOutsideIn(s.channelId)
      val new_s = Signal.changeId(s, innerChannelId)
      // TODO: change state
      // TODO: evaluate if TestEvent and/or ConstraintVariable should be set
      Reaction(Set(new_s), None, Set.empty)
    }
  }
}
