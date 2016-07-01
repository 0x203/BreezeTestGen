package de.hpi.asg.breezetestgen.domain.components.brzcomponents

import de.hpi.asg.breezetestgen.domain.components.{BrzComponent, BrzComponentBehaviour, HandshakeComponent}
import BrzComponent._

class UnaryFunc(id: HandshakeComponent.Id,
                operator: String,
                out: PullSpec,
                inp: PullSpec) extends BrzComponent(id) {
  require(List("Negate", "Invert") contains operator)

  type Behaviour = UnaryFuncBehaviour
  type C = UnaryFuncBehaviour.ControlState
  type D = Null

  def behaviour(state: Option[HandshakeComponent.State[C, D]]): Behaviour =
    new UnaryFuncBehaviour(state getOrElse UnaryFuncBehaviour.freshState)


  object UnaryFuncBehaviour {
    sealed trait ControlState
    case object Idle extends ControlState
    case object Fetching extends ControlState

    val freshState: HandshakeComponent.State[C, D] = HandshakeComponent.State(Idle, null)
  }

  class UnaryFuncBehaviour(initState: HandshakeComponent.State[C, D]) extends BrzComponentBehaviour[C, D](initState) {
    import UnaryFuncBehaviour._

    info(s"$id: UnaryFuncBehaviour created in state: $initState")

    when(Idle) {
      case Req(`out`, _) =>
        info(s"$id Requested out!")
        request(inp)
        goto(Fetching)
    }

    when(Fetching) {
      case DataAck(`inp`, inData, _) =>
        info(s"$id got $inData on input!")
        val outData = operator match {
          case "Invert" => inData.not()
          case "Negate" => throw new NotImplementedError() // TODO: implement me
        }
        dataAcknowledge(out, outData)
        goto(Idle)
    }
    initialize()
  }

}
