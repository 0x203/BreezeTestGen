package de.hpi.asg.breezetestgen.domain.components.brzcomponents

import de.hpi.asg.breezetestgen.domain.components.{BrzComponent, BrzComponentBehaviour, HandshakeComponent}
import BrzComponent._

class DecisionWait(id: HandshakeComponent.Id,
                   activate: SyncSpec,
                   inps: Seq[SyncSpec],
                   outs: Seq[SyncSpec]) extends BrzComponent(id) {
  require(inps.size == outs.size, s"$id DecisionWait: sizes of inps and outs must match")

  type Behaviour = DecisionWaitBehaviour
  type C = DecisionWaitBehaviour.ControlState
  type D = Null

  def behaviour(state: Option[HandshakeComponent.State[C, D]]): Behaviour =
    new DecisionWaitBehaviour(state getOrElse DecisionWaitBehaviour.freshState)


  object DecisionWaitBehaviour {
    sealed trait ControlState
    case object Idle extends ControlState
    case object Activated extends ControlState
    case object Executing extends ControlState

    val freshState: HandshakeComponent.State[C, D] = HandshakeComponent.State(Idle, null)
  }

  class DecisionWaitBehaviour(initState: HandshakeComponent.State[C, D]) extends BrzComponentBehaviour[C, D](initState) {
    import DecisionWaitBehaviour._

    info(s"$id DecisionWaitBehaviour created in state: $initState")

    when(Idle) {
      case Req(`activate`, _) =>
        info(s"$id Requested!")
        goto(Activated)
    }

    when(Activated) {
      case Req(inp, _) if inps contains inp =>
        val index = inps.indexOf(inp)
        info(s"$id was requested on input number $index")
        request(outs(index))
        goto(Executing)
    }

    when(Executing) {
      case Ack(out, _) if outs contains out =>
        val index = outs.indexOf(out)
        info(s"$id got acknowledge on output number $index")
        acknowledge(inps(index))
        goto(Idle)
    }

    initialize()
  }

}
