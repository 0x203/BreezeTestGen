package de.hpi.asg.breezetestgen.domain.components.brzcomponents

import de.hpi.asg.breezetestgen.domain.{Constant => DomainConstant}
import de.hpi.asg.breezetestgen.domain.components.BrzComponent._
import de.hpi.asg.breezetestgen.domain.components.{BrzComponent, BrzComponentBehaviour, HandshakeComponent}

class Constant(id: HandshakeComponent.Id,
               out: PullSpec,
               value: DomainConstant) extends BrzComponent(id) {
  type Behaviour = ConstantBehaviour
  type C = ConstantBehaviour.Idle.type
  type D = Null

  def behaviour(state: Option[HandshakeComponent.State[C, D]]): Behaviour =
    new ConstantBehaviour(state getOrElse ConstantBehaviour.freshState)


  object ConstantBehaviour {
    case object Idle
    val freshState: HandshakeComponent.State[C, D] = HandshakeComponent.State(Idle, null)
  }

  class ConstantBehaviour(initState: HandshakeComponent.State[C, D]) extends BrzComponentBehaviour[C, D](initState) {
    import ConstantBehaviour._

    info(s"$id: ConstantBehaviour created with value: $value")

    when(Idle) {
      case Req(`out`, _) =>
        info(s"$id: Requested ${value.value}!")
        dataAcknowledge(out, value)
        stay
    }

    initialize()
  }

}
