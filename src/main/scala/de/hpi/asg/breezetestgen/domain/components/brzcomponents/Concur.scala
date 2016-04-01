package de.hpi.asg.breezetestgen.domain.components.brzcomponents


import de.hpi.asg.breezetestgen.domain.components.{BrzComponent, BrzComponentBehaviour, HandshakeComponent}
import de.hpi.asg.breezetestgen.domain.components.BrzComponent._


class Concur(id: HandshakeComponent.Id,
             activate: SyncSpec,
             outs: Set[SyncSpec]) extends BrzComponent(id) {
  type Behaviour = ConcurBehaviour
  type C = ConcurBehaviour.ControlState
  type D = Option[Int]  // number of pending acknowledges

  def behaviour(state: Option[HandshakeComponent.State[C, D]]): Behaviour =
    new ConcurBehaviour(state getOrElse ConcurBehaviour.freshState)


  object ConcurBehaviour {
    sealed trait ControlState
    case object Idle extends ControlState
    case object Called extends ControlState

    val freshState: HandshakeComponent.State[ControlState, D] = HandshakeComponent.State(Idle, None)
  }

  class ConcurBehaviour(initState: HandshakeComponent.State[C, D]) extends BrzComponentBehaviour[C, D](initState) {
    import ConcurBehaviour._

    info(s"ConcurBehaviour created in state: $initState")

    when(Idle) {
      case Req(`activate`, _) =>
        info("Requested!")
        outs.foreach{request(_)}
        goto(Called) using Some(outs.size)
    }

    when(Called) {
      case Ack(out, Some(i)) if outs contains out =>
        if (i != 1) {
          info(s"Channel acknowledged. ${i - 1} still pending...")
          stay using Option(i - 1)
        } else {
          info("all finished. acknowledging...")
          acknowledge(activate)
          goto(Idle)
        }
    }

    initialize()
  }
}