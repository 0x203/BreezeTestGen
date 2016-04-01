package de.hpi.asg.breezetestgen.domain.components.brzcomponents

import de.hpi.asg.breezetestgen.domain.{Constant, Data}
import de.hpi.asg.breezetestgen.domain.components.BrzComponent._
import de.hpi.asg.breezetestgen.domain.components.{BrzComponent, BrzComponentBehaviour, HandshakeComponent}

class BinaryFuncConstR(id: HandshakeComponent.Id,
                       operator: String,
                       bData: Constant,
                       out: PullSpec,
                       inpA: PullSpec) extends BrzComponent(id) {
  type Behaviour = BinaryFuncConstRBehaviour
  type C = BinaryFuncConstRBehaviour.ControlState
  type D = Null

  def behaviour(state: Option[HandshakeComponent.State[C, D]]): Behaviour =
    new BinaryFuncConstRBehaviour(state getOrElse BinaryFuncConstRBehaviour.freshState)

  private def operate: (Data) => Data = BinaryFunc.operate(operator)(_, bData)

  object BinaryFuncConstRBehaviour {
    sealed trait ControlState
    case object Idle extends ControlState
    case object Waiting extends ControlState

    val freshState: HandshakeComponent.State[ControlState, D] = HandshakeComponent.State(Idle, null)
  }

  class BinaryFuncConstRBehaviour(initState: HandshakeComponent.State[C, D]) extends BrzComponentBehaviour[C, D](initState) {
    import BinaryFuncConstRBehaviour._

    info(s"BinaryFuncConstRBehaviour created in state:  $initState")

    when(Idle) {
      case Req(`out`, _) =>
        info("Activated. Requesting A...")
        request(inpA)
        goto(Waiting)
    }

    when(Waiting) {
      case DataAck(`inpA`, aData, _) =>
        info(s"Got data on inpA: $aData")
        //TODO: add constraints here
        dataAcknowledge(out, operate(aData))
        goto(Idle)
    }

    initialize()
  }

}
