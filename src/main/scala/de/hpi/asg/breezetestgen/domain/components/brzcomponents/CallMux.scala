package de.hpi.asg.breezetestgen.domain.components.brzcomponents

import de.hpi.asg.breezetestgen.domain.components.BrzComponent._
import de.hpi.asg.breezetestgen.domain.components.{BrzComponent, BrzComponentBehaviour, HandshakeComponent}

class CallMux(id: HandshakeComponent.Id,
              inps: Set[PushSpec],
              out: PushSpec) extends BrzComponent(id) {
  type Behaviour = CallMuxBehaviour
  type C = CallMuxBehaviour.ControlState
  type D = Option[PushSpec]   // writer (inp) to acknowledge

  def behaviour(state: Option[HandshakeComponent.State[C, D]]): Behaviour =
    new CallMuxBehaviour(state getOrElse CallMuxBehaviour.freshState)


  object CallMuxBehaviour {
    sealed trait ControlState
    case object Idle extends ControlState
    case object Called extends ControlState

    val freshState: HandshakeComponent.State[ControlState, D] = HandshakeComponent.State(Idle, None)
  }

  class CallMuxBehaviour(initState: HandshakeComponent.State[C, D]) extends BrzComponentBehaviour[C, D](initState) {

    import CallMuxBehaviour._

    info(s"$id: CallMuxBehaviour created in state:  $initState")

    when(Idle) {
      case DataReq(inp, data, _) if inps contains inp =>
        info(s"$id: Requested!")
        dataRequest(out, data)
        goto(Called) using Some(inp)
    }

    when(Called) {
      case Ack(`out`, Some(writer)) =>
        info(s"$id: Acknowledged")
        acknowledge(writer)
        goto(Idle) using None
    }

    initialize()
  }

}
