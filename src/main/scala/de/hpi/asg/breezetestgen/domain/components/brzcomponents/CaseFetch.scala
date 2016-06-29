package de.hpi.asg.breezetestgen.domain.components.brzcomponents

import de.hpi.asg.breezetestgen.domain.components.{BrzComponent, BrzComponentBehaviour, HandshakeComponent}
import BrzComponent._

class CaseFetch(id: HandshakeComponent.Id,
                selector: Case.Selector,
                out: PullSpec,
                index: PullSpec,
                inps: Seq[PullSpec]) extends BrzComponent(id) {
  type Behaviour = CaseFetchBehaviour
  type C = CaseFetchBehaviour.ControlState
  type D = Null

  def behaviour(state: Option[HandshakeComponent.State[C, D]]): Behaviour =
    new CaseFetchBehaviour(state getOrElse CaseFetchBehaviour.freshState)


  object CaseFetchBehaviour {
    sealed trait ControlState
    case object Idle extends ControlState
    case object Evaluating extends ControlState
    case object Fetching extends ControlState

    val freshState: HandshakeComponent.State[C, D] = HandshakeComponent.State(Idle, null)
  }

  class CaseFetchBehaviour(initState: HandshakeComponent.State[C, D]) extends BrzComponentBehaviour[C, D](initState) {
    import CaseFetchBehaviour._

    info(s"$id: CaseFetchBehaviour created in state: $initState")

    when(Idle) {
      case Req(`out`, _) =>
        info(s"$id: Requested! Will fetch index...")
        request(index)
        goto(Evaluating)
    }

    when(Evaluating) {
      case DataAck(`index`, data, _) =>
        info(s"$id: got index data $data")
        val possibilities = selector.usableValueConstraints(data).mapValues{ index => () => {
          val inp = inps(index)
          info(s"$id: Index $index maps to channel $inp")
          request(inp)
          goto(Fetching)
        }}.view.force

        //TODO fix selector and use same solution as case for possibility of "none matched"

        decideBetween(possibilities) getOrElse stay
    }

    when(Fetching) {
      case DataAck(inp, data, _) if inps contains inp =>
        info(s"$id: Got data to return...")
        dataAcknowledge(out, data)
        goto(Idle)
    }

    initialize()
  }

}
