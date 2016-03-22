package de.hpi.asg.breezetestgen.domain.components

import de.hpi.asg.breezetestgen.domain._
import Channel._

object Fetch {
  val breezeName = "BrzFetch"
}

class Fetch[DT <: Data](activate: Spec[SyncChannel[_]],
                        inp: Spec[PullChannel[_]],
                        out: Spec[PushChannel[_]]) extends BrzComponent[DT] {
  type Behaviour = FetchBehaviour
  type C = FetchBehaviour.ControlState
  type D = Null

  def behaviour(state: Option[ComponentState[C, D]]): Behaviour =
    new FetchBehaviour(state getOrElse FetchBehaviour.freshState)


  object FetchBehaviour {
    sealed trait ControlState
    case object Idle extends ControlState
    case object WaitForInp extends ControlState
    case object WaitForOut extends ControlState

    val freshState: ComponentState[ControlState, Null] = ComponentState(Idle, null)
  }

  class FetchBehaviour(initState: ComponentState[C, D]) extends ComponentBehaviour[DT, C, D](initState) {
    import FetchBehaviour._

    when(Idle) {
      case Req(`activate`, _) =>
        //info("Activated")
        request(inp)
        goto(WaitForInp)
    }

    when(WaitForInp) {
      case DataAck(`inp`, data, _) =>
        //info(s"read ${data.value}")
        //TODO: add constraints for data
        dataRequest(out, data)
        goto(WaitForOut)
    }

    when(WaitForOut) {
      case Ack(`out`, _) =>
        //info("wrote")
        acknowledge(activate)
        goto(Idle)
    }

    initialize()
  }
}
