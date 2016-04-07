package de.hpi.asg.breezetestgen.domain.components.brzcomponents

import de.hpi.asg.breezetestgen.domain.{Constant, Data}
import de.hpi.asg.breezetestgen.domain.components.BrzComponent._
import de.hpi.asg.breezetestgen.domain.components.{BrzComponent, BrzComponentBehaviour, HandshakeComponent}

object Case {
  case class Selector(indices: Map[Int, Int], ranges: Map[Range, Int], left: Set[Int]) {
    private def equalConstraint(reference: Data, value: Int): Data.ConstraintOrBool =
      (reference === Constant(value)).isTruthy

    def usableValueConstraints(reference: Data): Map[Data.ConstraintOrBool, Int] = {
      val directMaps = indices.map{case (value, index) =>
        equalConstraint(reference, value) -> index
      }
      //TODO: find better way than enumeration, e.g. two combined constraints (greaterThan start, lessThan end)
      val rangesEnumerated: Map[Data.ConstraintOrBool, Int] = for {
        (range, index) <- ranges
        value <- range
      } yield equalConstraint(reference, value) -> index

      directMaps ++ rangesEnumerated
    }

    //TODO: find better way for this, too
    def unusableConstraints(reference: Data): Set[Data.ConstraintOrBool] = left.map(equalConstraint(reference, _))
  }
}

class Case(id: HandshakeComponent.Id,
           selector: Case.Selector,
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
        val possibilities = selector.usableValueConstraints(data).mapValues{ index => () => {
          val out = outs(index)
          info(s"Index $index maps to channel $out")
          request(out)
          goto(Called)
        }}.view.force

        val alternative = selector.unusableConstraints(data).map{c => c -> (() => {
          info("No index matched this constant")
          acknowledge(inp)
          stay
        })}

        decideBetween(possibilities ++ alternative) getOrElse stay
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
