package de.hpi.asg.breezetestgen.domain.components.brzcomponents

import de.hpi.asg.breezetestgen.domain.Constant
import de.hpi.asg.breezetestgen.domain.components.BrzComponent._
import de.hpi.asg.breezetestgen.domain.components.{BrzComponent, BrzComponentBehaviour, HandshakeComponent}

object Case {
  type SelectorSpec = Int => Option[Int]
}

class Case(id: HandshakeComponent.Id,
           selectorSpec: Case.SelectorSpec,
           inp: PushSpec,
           outs: Seq[SyncSpec]) extends BrzComponent(id) {
  type Behaviour = CaseBehaviour
  type C = CaseBehaviour.ControlState
  type D = Null

  def behaviour(state: Option[HandshakeComponent.State[C, D]]): Behaviour =
    new CaseBehaviour(state getOrElse CaseBehaviour.freshState)


  object CaseBehaviour {
    sealed trait ControlState
    case object Idle extends ControlState
    case object Called extends ControlState

    val freshState: HandshakeComponent.State[ControlState, D] = HandshakeComponent.State(Idle, null)
  }

  class CaseBehaviour(initState: HandshakeComponent.State[C, D]) extends BrzComponentBehaviour[C, D](initState) {
    import CaseBehaviour._

    info(s"CaseBehaviour created in state:  $initState")

    when(Idle) {
      case DataReq(`inp`, data, _) =>
        info(s"Activated with data $data")
        data match {
          case c: Constant =>
            selectorSpec(c.value) match {
              case Some(index) =>
                val out = outs(index)
                info(s"Index $index maps to channel $out")
                request(out)
                goto(Called)
              case None =>
                info("No index matched this constant")
                acknowledge(inp)
                stay
            }
          case _ =>
            // TODO: let central instance decide what to do
            throw new NotImplementedError("Case can only handle Constants for now!")
        }
    }

    when(Called) {
      case Ack(out, _) if outs contains out =>
        info("Returning...")
        acknowledge(inp)
        goto(Idle)
    }

    initialize()
  }

}
