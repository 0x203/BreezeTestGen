package de.hpi.asg.breezetestgen.domain.components.brzcomponents

import de.hpi.asg.breezetestgen.domain.components.{BrzComponent, BrzComponentBehaviour, HandshakeComponent}
import BrzComponent._

class Call(id: HandshakeComponent.Id,
           inps: Set[SyncSpec],
           out: SyncSpec) extends BrzComponent(id) {
  type Behaviour = CallBehaviour
  type C = CallBehaviour.ControlState
  type D = Option[SyncSpec] // calling input

  def behaviour(state: Option[HandshakeComponent.State[C, D]]): Behaviour =
    new CallBehaviour(state getOrElse CallBehaviour.freshState)


  object CallBehaviour {
    sealed trait ControlState
    case object Idle extends ControlState
    case object Called extends ControlState

    val freshState: HandshakeComponent.State[C, D] = HandshakeComponent.State(Idle, None)
  }

  class CallBehaviour(initState: HandshakeComponent.State[C, D]) extends BrzComponentBehaviour[C, D](initState) {
    import CallBehaviour._

    info(s"$id: CallBehaviour created in state: $initState")

    when(Idle) {
      case Req(inp, _) if inps contains inp =>
        info(s"$id Requested on channel $inp!")
        request(out)
        goto(Called) using Some(inp)
    }

    when(Called) {
      case Ack(`out`, Some(inp)) =>
        info(s"$id: Got ack from out, forward it to $inp")
        acknowledge(inp)
        goto(Idle) using None
    }

    initialize()
  }
}
