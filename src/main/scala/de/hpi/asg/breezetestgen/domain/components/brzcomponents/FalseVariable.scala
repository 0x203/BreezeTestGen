package de.hpi.asg.breezetestgen.domain.components.brzcomponents

import de.hpi.asg.breezetestgen.domain.components.{BrzComponent, BrzComponentBehaviour, HandshakeComponent}
import BrzComponent._
import de.hpi.asg.breezetestgen.domain.Data

class FalseVariable(id: HandshakeComponent.Id,
                    readerSpec: Variable.ReaderSpec,
                    write: PushSpec,
                    signal: SyncSpec,
                    reads: Seq[PullSpec]) extends BrzComponent(id) {
  type Behaviour = FalseVariableBehaviour
  type C = FalseVariableBehaviour.ControlState
  type D = Option[Data]

  def behaviour(state: Option[HandshakeComponent.State[C, D]]): Behaviour =
    new FalseVariableBehaviour(state getOrElse FalseVariableBehaviour.freshState)


  object FalseVariableBehaviour {
    sealed trait ControlState
    case object Idle extends ControlState
    case object Active extends ControlState

    val freshState: HandshakeComponent.State[C, D] = HandshakeComponent.State(Idle, None)
  }

  class FalseVariableBehaviour(initState: HandshakeComponent.State[C, D]) extends BrzComponentBehaviour[C, D](initState) {
    import FalseVariableBehaviour._

    info(s"$id: FalseVariableBehaviour created in state: $initState")

    when(Idle) {
      case DataReq(`write`, newData, _) =>
        info(s"Got Data: $newData!")
        request(signal)
        goto(Active) using Option(newData)
    }

    when(Active) {
      case Ack(`signal`, _) =>
        info(s"$id: isch over, will acknowledge")
        acknowledge(write)
        goto(Idle) using None

      case Req(reader, Some(data)) if reads contains reader =>
        info(s"$id: reader #${reads.indexOf(reader)} wants to read me")
        val filteredData: Data = readerSpec(reads.indexOf(reader)) match {
          case Some(range) if range.isEmpty => data
          case Some(range) => data.selectBits(range)
          case None =>
            error("could not find specified range!")
            data
        }
        info(s"$filteredData read from $id")
        dataAcknowledge(reader, filteredData)
        stay
    }

    initialize()
  }

}
