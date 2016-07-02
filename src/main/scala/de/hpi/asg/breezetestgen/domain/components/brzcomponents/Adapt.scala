package de.hpi.asg.breezetestgen.domain.components.brzcomponents

import de.hpi.asg.breezetestgen.domain.components.{BrzComponent, BrzComponentBehaviour, HandshakeComponent}
import BrzComponent._
import de.hpi.asg.breezetestgen.domain.Data

class Adapt(id: HandshakeComponent.Id,
            outWidth: Int,
            inWidth: Int,
            outSigned: Boolean,
            inSigned: Boolean,
            out: PullSpec,
            inp: PullSpec) extends BrzComponent(id) {
  type Behaviour = AdaptBehaviour
  type C = AdaptBehaviour.ControlState
  type D = Null

  def behaviour(state: Option[HandshakeComponent.State[C, D]]): Behaviour =
    new AdaptBehaviour(state getOrElse AdaptBehaviour.freshState)


  object AdaptBehaviour {
    sealed trait ControlState
    case object Idle extends ControlState
    case object Fetching extends ControlState

    val freshState: HandshakeComponent.State[C, D] = HandshakeComponent.State(Idle, null)
  }

  class AdaptBehaviour(initState: HandshakeComponent.State[C, D]) extends BrzComponentBehaviour[C, D](initState) {
    import AdaptBehaviour._

    info(s"$id AdaptBehaviour created in state: $initState")

    when(Idle) {
      case Req(`out`, _) =>
        info(s"$id: Requested!")
        request(inp)
        goto(Fetching)
    }

    when(Fetching) {
      case DataAck(`inp`, inData, _) =>
        info(s"$id Got input data: $inData")

        if ((inData.bitCount != inWidth) | (inData.isSigned != inSigned))
          error(s"$id: Expecting bitcount $inWidth and signed $inSigned; got ${inData.bitCount} and ${inData.isSigned}")

        val outData = inData.adapt(outWidth, outSigned, inWidth, inSigned)
        dataAcknowledge(out, outData)
        goto(Idle)
    }

    initialize()
  }

}
