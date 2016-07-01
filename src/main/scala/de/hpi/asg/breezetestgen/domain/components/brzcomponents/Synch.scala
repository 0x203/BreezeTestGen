package de.hpi.asg.breezetestgen.domain.components.brzcomponents

import de.hpi.asg.breezetestgen.domain.components.{BrzComponent, BrzComponentBehaviour, HandshakeComponent}
import BrzComponent._

class Synch(id: HandshakeComponent.Id,
            inps: Set[SyncSpec],
            out: SyncSpec) extends BrzComponent(id) {
  type Behaviour = SynchBehaviour
  type C = SynchBehaviour.ControlState
  type D = Int

  def behaviour(state: Option[HandshakeComponent.State[C, D]]): Behaviour =
    new SynchBehaviour(state getOrElse SynchBehaviour.freshState)


  object SynchBehaviour {
    sealed trait ControlState
    case object Waiting extends ControlState
    case object Calling extends ControlState

    val freshState: HandshakeComponent.State[C, D] = HandshakeComponent.State(Waiting, 0)
  }

  class SynchBehaviour(initState: HandshakeComponent.State[C, D]) extends BrzComponentBehaviour[C, D](initState) {
    import SynchBehaviour._

    info(s"$id: SyncBehaviour created in state: $initState")

    when(Waiting) {
      case Req(inp, oldReqCount) if inps contains inp =>
        val reqCount = oldReqCount + 1
        if (reqCount == inps.size) {
          info(s"$id: got all requests, activating `out` now")
          request(out)
          goto(Calling) using 0
        } else {
          info(s"$id: got $reqCount of ${inps.size} requests")
          stay using reqCount
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
