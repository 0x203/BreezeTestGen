package de.hpi.asg.breezetestgen.domain.components.brzcomponents

import de.hpi.asg.breezetestgen.domain.components.{BrzComponent, BrzComponentBehaviour, HandshakeComponent}
import BrzComponent._
import de.hpi.asg.breezetestgen.domain.Data

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

  def invert(data: Data): Data = data.not()
  def negate(data: Data): Data = throw new NotImplementedError() //TODO: implement me

  val func: Data => Data = operator match {
    case "Invert" => invert _
    case "Negate" => negate _
  }


  object UnaryFuncBehaviour {
    sealed trait ControlState
    case object Idle extends ControlState
    case object Fetching extends ControlState

    val freshState: HandshakeComponent.State[C, D] = HandshakeComponent.State(Idle, null)
  }

  class UnaryFuncBehaviour(initState: HandshakeComponent.State[C, D]) extends BrzComponentBehaviour[C, D](initState) {
    import UnaryFuncBehaviour._

    info(s"$id: UnaryFuncBehaviour $operator created in state: $initState")

    when(Idle) {
      case Req(`out`, _) =>
        info(s"$id Requested out!")
        request(inp)
        goto(Fetching)
    }

    when(Fetching) {
      case DataAck(`inp`, inData, _) =>
        info(s"$id got $inData on input!")
        val outData = func(inData)
        dataAcknowledge(out, outData)
        goto(Idle)
    }
    initialize()
  }

}
