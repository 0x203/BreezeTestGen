package de.hpi.asg.breezetestgen.domain.components.brzcomponents


import de.hpi.asg.breezetestgen.domain.components.{BrzComponent, BrzComponentBehaviour, HandshakeComponent}
import de.hpi.asg.breezetestgen.domain.components.BrzComponent._
import de.hpi.asg.breezetestgen.testing.TestEvent


class Concur(id: HandshakeComponent.Id,
             activate: SyncSpec,
             outs: Set[SyncSpec]) extends BrzComponent(id) {
  type Behaviour = ConcurBehaviour
  type C = ConcurBehaviour.ControlState
  type D = Option[ConcurBehaviour.Requests]  // number of pending acknowledges

  def behaviour(state: Option[HandshakeComponent.State[C, D]]): Behaviour =
    new ConcurBehaviour(state getOrElse ConcurBehaviour.freshState)


  object ConcurBehaviour {
    sealed trait ControlState
    case object Idle extends ControlState
    case object Called extends ControlState

    case class Requests(pending: Int, preTestEvent: TestEvent, testEvents: Set[TestEvent])

    val freshState: HandshakeComponent.State[ControlState, D] = HandshakeComponent.State(Idle, None)
  }

  class ConcurBehaviour(initState: HandshakeComponent.State[C, D]) extends BrzComponentBehaviour[C, D](initState) {
    import ConcurBehaviour._

    info(s"$id: ConcurBehaviour created in state: $initState")

    when(Idle) {
      case Req(`activate`, _) =>
        info("$id: Requested!")
        outs.foreach{request(_)}
        goto(Called) using Some(Requests(outs.size, testEvent, Set.empty))
    }

    when(Called) {
      case Ack(out, Some(Requests(i, preTestEvent, testEvents))) if outs contains out =>
        val newTestEvents = testEvents + testEvent
        if (i != 1) {
          info(s"$id: Channel acknowledged. ${i - 1} still pending...")
          stay using Option(Requests(i - 1, preTestEvent, newTestEvents))
        } else {
          info(s"$id: all finished. acknowledging...")
          acknowledge(activate)
          mergeAfter(newTestEvents - preTestEvent)
          goto(Idle)
        }
    }

    initialize()
  }
}