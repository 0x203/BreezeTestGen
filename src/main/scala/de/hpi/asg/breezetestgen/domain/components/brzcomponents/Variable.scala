package de.hpi.asg.breezetestgen.domain.components.brzcomponents

import de.hpi.asg.breezetestgen.domain.Data
import de.hpi.asg.breezetestgen.domain.components.BrzComponent._
import de.hpi.asg.breezetestgen.domain.components.brzcomponents.Variable.ReaderSpec
import de.hpi.asg.breezetestgen.domain.components.{BrzComponent, BrzComponentBehaviour, HandshakeComponent}

object Variable {
  type ReaderSpec = (Int => Option[Range])
}

class Variable(id: HandshakeComponent.Id,
               name: String,
               readerSpec: ReaderSpec,
               write: PushSpec,
               reads: Seq[PullSpec]) extends BrzComponent(id) {
  type Behaviour = VariableBehaviour
  type C = VariableBehaviour.ControlState
  type D = Option[Data]

  def behaviour(state: Option[HandshakeComponent.State[C, D]]): Behaviour =
    new VariableBehaviour(state getOrElse VariableBehaviour.freshState)


  object VariableBehaviour {
    sealed trait ControlState
    case object Idle extends ControlState

    val freshState: HandshakeComponent.State[ControlState, D] = HandshakeComponent.State(Idle, None)
  }

  class VariableBehaviour(initState: HandshakeComponent.State[C, D]) extends BrzComponentBehaviour[C, D](initState) {

    import VariableBehaviour._

    info(s"$id: VariableBehaviour created in state:  $initState")

    when(Idle) {
      case DataReq(`write`, data, _) =>
        info(s"$name is written: $data")
        acknowledge(write)
        stay using Option(data)

      case Req(reader, Some(data)) if reads contains reader =>
        val filteredData: Data = readerSpec(reads.indexOf(reader)) match {
          case Some(range) if range.isEmpty => data
          case Some(range) =>
            info(s"$id: selecting $range of bits from data $data")
            data.selectBits(range)
          case None =>
            error(s"$id: could not find specified range!")
            data
        }
        info(s"$id: $name is read: $filteredData")
        dataAcknowledge(reader, filteredData)
        stay
    }

    initialize()
  }

}
