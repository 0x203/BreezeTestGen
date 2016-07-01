package de.hpi.asg.breezetestgen.domain.components.brzcomponents

import de.hpi.asg.breezetestgen.domain.components.{BrzComponent, BrzComponentBehaviour, HandshakeComponent}
import BrzComponent._

class Sync(id: HandshakeComponent.Id,
           inps: Set[SyncSpec],
           out: SyncSpec) extends BrzComponent(id) {
  type Behaviour = SyncBehaviour
  type C = SyncBehaviour.ControlState
  type D = Int

  def behaviour(state: Option[HandshakeComponent.State[C, D]]): Behaviour =
    new SyncBehaviour(state getOrElse SyncBehaviour.freshState)


  object SyncBehaviour {
    sealed trait ControlState
    case object Waiting extends ControlState
    case object Calling extends ControlState

    val freshState: HandshakeComponent.State[C, D] = HandshakeComponent.State(Waiting, 0)
  }

  class SyncBehaviour(initState: HandshakeComponent.State[C, D]) extends BrzComponentBehaviour[C, D](initState) {
    import SyncBehaviour._

    info(s"$id: SyncBehaviour created in state: $initState")

    when(Waiting) {
      case Req(inp, reqCount) if inps contains inp =>
        if (reqCount == inps.size) {
          info(s"$id: got all requests, activating `out` now")
          request(out)
          goto(Calling) using 0
        } else {
          info(s"$id: got ${reqCount + 1} of ${inps.size} requests")
          stay using reqCount + 1
        }
    }

    when(Calling) {
      case Ack(`out`, _) =>
        for (inp <- inps)
          acknowledge(inp)
        goto(Waiting) using 0
    }

    initialize()
  }

}
