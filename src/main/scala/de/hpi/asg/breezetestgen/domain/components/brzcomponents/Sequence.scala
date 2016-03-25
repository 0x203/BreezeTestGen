package de.hpi.asg.breezetestgen.domain.components.brzcomponents

import de.hpi.asg.breezetestgen.domain.components.BrzComponent._
import de.hpi.asg.breezetestgen.domain.components.{BrzComponent, BrzComponentBehaviour, HandshakeComponent}

object Sequence {
  val breezeName = "BrzSequence"
}

class Sequence(id: HandshakeComponent.Id,
               activate: SyncSpec,
               outs: Seq[SyncSpec]) extends BrzComponent(id) {
  type Behaviour = SequenceBehaviour
  type C = SequenceBehaviour.ControlState
  type D = Option[Int]

  def behaviour(state: Option[HandshakeComponent.State[C, D]]): Behaviour =
    new SequenceBehaviour(state getOrElse SequenceBehaviour.freshState)


  object SequenceBehaviour {
    sealed trait ControlState
    case object Idle extends ControlState
    case object Called extends ControlState

    val freshState: HandshakeComponent.State[ControlState, D] = HandshakeComponent.State(Idle, None)
  }

  class SequenceBehaviour(initState: HandshakeComponent.State[C, D]) extends BrzComponentBehaviour[C, D](initState) {
    import SequenceBehaviour._

    info(s"SequenceBehaviour created in state: $initState")

    when(Idle) {
      case Req(`activate`, _) =>
        info("Requested!")
        request(outs.head)
        goto(Called) using Some(0)
    }

    when(Called) {
      case Ack(out, Some(i)) if out == outs(i) =>
        val next_i = i + 1
        if (next_i == outs.length) {
          acknowledge(activate)
          info("all finished. acknowledging...")
          goto(Idle)
        } else {
          request(outs(next_i))
          info(s"$next_i of ${outs.length} finished. requesting next...")
          stay using Some(next_i)
        }
    }

    initialize()
  }
}
