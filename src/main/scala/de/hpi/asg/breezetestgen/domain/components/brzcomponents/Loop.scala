package de.hpi.asg.breezetestgen.domain.components.brzcomponents

import de.hpi.asg.breezetestgen.domain.components.{BrzComponent, BrzComponentBehaviour, HandshakeComponent}
import BrzComponent._

class Loop(id: HandshakeComponent.Id,
           activate: SyncSpec,
           out: SyncSpec) extends BrzComponent(id) {
  type Behaviour = LoopBehaviour
  type C = LoopBehaviour.ControlState
  type D = Null

  def behaviour(state: Option[HandshakeComponent.State[C, D]]): Behaviour =
    new LoopBehaviour(state getOrElse LoopBehaviour.freshState)


  object LoopBehaviour {
    sealed trait ControlState
    case object Idle extends ControlState
    case object Called extends ControlState

    val freshState: HandshakeComponent.State[C, D] = HandshakeComponent.State(Idle, null)
  }

  class LoopBehaviour(initState: HandshakeComponent.State[C, D]) extends BrzComponentBehaviour[C, D](initState) {
    import LoopBehaviour._

    info(s"LoopBehaviour created in state: $initState")

    when(Idle) {
      case Req(`activate`, _) =>
        info(s"Requested!")
        request(out)
        goto(Called)
    }

    when(Called) {
      case Ack(`out`, _) =>
        info("Acknowledged! Requesting again...")
        request(out)
        stay
    }

    initialize()
  }

}
