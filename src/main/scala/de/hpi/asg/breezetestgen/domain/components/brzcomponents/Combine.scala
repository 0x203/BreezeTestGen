package de.hpi.asg.breezetestgen.domain.components.brzcomponents

import de.hpi.asg.breezetestgen.domain.components.{BrzComponent, BrzComponentBehaviour, HandshakeComponent}
import BrzComponent._
import de.hpi.asg.breezetestgen.domain.Data
import org.w3c.dom.ls.LSInput

class Combine(id: HandshakeComponent.Id,
              outWidth: Int,
              lsInputWidth: Int,
              msInputWidth: Int,
              out: PullSpec,
              lsInp: PullSpec,
              msInp: PullSpec) extends BrzComponent(id) {
  type Behaviour = CombineBehaviour
  type C = CombineBehaviour.ControlState
  type D = Option[Data]

  if(lsInputWidth + msInputWidth != outWidth)
    error(s"$id: withs don't match: $lsInputWidth + $msInputWidth != $outWidth")

  def behaviour(state: Option[HandshakeComponent.State[C, D]]): Behaviour =
    new CombineBehaviour(state getOrElse CombineBehaviour.freshState)

  def combine(lsData: Data, msData: Data): Data = {
    if(lsData.bitCount != lsInputWidth)
      error(s"got ${lsData.bitCount} bits on lsInp, expected $lsInputWidth")
    else if(msData.bitCount != msInputWidth)
      error(s"got ${msData.bitCount} bits on msInp, expected $msInputWidth")

    lsData.combineWithMoreSignificant(msData)
  }

  object CombineBehaviour {
    sealed trait ControlState
    case object Idle extends ControlState
    case object Fetching extends ControlState

    val freshState: HandshakeComponent.State[C, D] = HandshakeComponent.State(Idle, None)
  }

  class CombineBehaviour(initState: HandshakeComponent.State[C, D]) extends BrzComponentBehaviour[C, D](initState) {
    import CombineBehaviour._

    info(s"$id: CombineBehaviour created in state: $initState")

    when(Idle) {
      case Req(`out`, _) =>
        info(s"Requested!")
        request(lsInp)
        request(msInp)
        goto(Fetching) using None
    }

    when(Fetching) {
      // when the first one arrives, store it's data and wait for other
      case DataAck(`lsInp`, lsData, None) => stay using Some(lsData)
      case DataAck(`msInp`, msData, None) => stay using Some(msData)

      // when the second one arrives, return combined data
      case DataAck(`lsInp`, lsData, Some(msData)) =>
        dataAcknowledge(out, combine(lsData, msData))
        goto(Idle) using None
      case DataAck(`msInp`, msData, Some(lsData)) =>
        dataAcknowledge(out, combine(lsData, msData))
        goto(Idle) using None
    }

    initialize()
  }

}
