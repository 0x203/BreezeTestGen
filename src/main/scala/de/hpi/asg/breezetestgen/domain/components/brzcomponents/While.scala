package de.hpi.asg.breezetestgen.domain.components.brzcomponents

import de.hpi.asg.breezetestgen.domain.components.BrzComponent._
import de.hpi.asg.breezetestgen.domain.components.{BrzComponent, BrzComponentBehaviour, HandshakeComponent}

class While(id: HandshakeComponent.Id,
            activate: SyncSpec,
            guard: PullSpec,
            out: SyncSpec) extends BrzComponent(id) {
  type Behaviour = WhileBehaviour
  type C = WhileBehaviour.ControlState
  type D = Null

  def behaviour(state: Option[HandshakeComponent.State[C, D]]): Behaviour =
    new WhileBehaviour(state getOrElse WhileBehaviour.freshState)


  object WhileBehaviour {
    sealed trait ControlState
    case object Idle extends ControlState
    case object Evaluating extends ControlState
    case object Executing extends ControlState

    val freshState: HandshakeComponent.State[ControlState, D] = HandshakeComponent.State(Idle, null)
  }

  class WhileBehaviour(initState: HandshakeComponent.State[C, D]) extends BrzComponentBehaviour[C, D](initState) {

    import WhileBehaviour._

    info(s"WhileBehaviour created in state:  $initState")

    when(Idle) {
      case Req(`activate`, _) =>
        info("Requested!")
        request(guard)
        goto(Evaluating)
    }

    when(Evaluating) {
      case DataAck(`guard`, x, _) =>
        decideBetween(Map(
          x.isFalsy -> (() => {
            info("guard was false. returning...")
            acknowledge(activate)
            goto(Idle)
          }),
          x.isTruthy -> (() => {
            info("guard was true. entering loop...")
            request(out)
            goto(Executing)
          })
        )) getOrElse stay
    }

    when(Executing) {
      case Ack(`out`, _) =>
        info("Loop body finished. Evaluating again...")
        request(guard)
        goto(Evaluating)
    }

    initialize()
  }

}
