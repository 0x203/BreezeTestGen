package de.hpi.asg.breezetestgen.domain.components.brzcomponents

import de.hpi.asg.breezetestgen.Loggable
import de.hpi.asg.breezetestgen.domain.Data
import de.hpi.asg.breezetestgen.domain.components.BrzComponent._
import de.hpi.asg.breezetestgen.domain.components.{BrzComponent, BrzComponentBehaviour, HandshakeComponent}
import de.hpi.asg.breezetestgen.testing.TestEvent

object BinaryFunc extends Loggable {
  def operate(operator: String)(a: Data, b: Data): Data = {
    val res = operator match {
      case "Add" => a + b
      case "Subtract" => a - b
      case "Equals" => a === b
      case "NotEquals" => a !== b
      case "LessThan" => a < b
      case "GreaterThan" => a > b
      case "LessOrEquals" => a <= b
      case "GreaterOrEquals" => a >= b
      case "And" => a & b
      case "Or" => a | b
      case "Xor" => a ^ b
    }
    info(s"$a $operator $b = $res")
    res
  }
}

class BinaryFunc(id: HandshakeComponent.Id,
                 operator: String,
                 out: PullSpec,
                 inpA: PullSpec,
                 inpB: PullSpec) extends BrzComponent(id) {
  type Behaviour = BinaryFuncBehaviour
  type C = BinaryFuncBehaviour.ControlState
  type D = BinaryFuncBehaviour.InputsAndTestEvent

  def behaviour(state: Option[HandshakeComponent.State[C, D]]): Behaviour =
    new BinaryFuncBehaviour(state getOrElse BinaryFuncBehaviour.freshState)

  private def operate: (Data, Data) => Data = BinaryFunc.operate(operator)

  object BinaryFuncBehaviour {
    sealed trait ControlState
    case object Idle extends ControlState
    case object Waiting extends ControlState

    final case class InputsAndTestEvent(a: Option[Data], b: Option[Data], teO: Set[TestEvent])
    object InputsAndTestEvent {
      def fresh(): InputsAndTestEvent = InputsAndTestEvent(None, None, Set.empty)
    }

    val freshState: HandshakeComponent.State[ControlState, InputsAndTestEvent] =
      HandshakeComponent.State(Idle, InputsAndTestEvent.fresh())
  }

  class BinaryFuncBehaviour(initState: HandshakeComponent.State[C, D]) extends BrzComponentBehaviour[C, D](initState) {
    import BinaryFuncBehaviour._

    info(s"BinaryFuncBehaviour created in state:  $initState")

    def inputsAvailable(a: Data, b: Data, tes: Set[TestEvent]): State = {
      mergeAfter(tes)
      dataAcknowledge(out, operate(a, b))
      goto(Idle)
    }

    when(Idle) {
      case Req(`out`, _) =>
        info("Activated. Requesting A & B...")
        request(inpA)
        request(inpB)
        goto(Waiting) using InputsAndTestEvent.fresh()
    }

    when(Waiting) {
      case DataAck(`inpA`, aData, InputsAndTestEvent(None, b, tes)) =>
        val newTEs = tes + testEvent
        info(s"Got data on inpA: $aData")
        b match {
          case Some(bData) => inputsAvailable(aData, bData, tes)
          case None =>
            stay using InputsAndTestEvent(Some(aData), None, newTEs)
        }

      case DataAck(`inpB`, bData, InputsAndTestEvent(a, None, tes)) =>
        val newTEs = tes + testEvent
        info(s"Got data on inpB: $bData")
        a match {
          case Some(aData) => inputsAvailable(aData, bData, tes)
          case None =>
            stay using InputsAndTestEvent(None, Some(bData), newTEs)
        }
    }

    initialize()
  }

}
