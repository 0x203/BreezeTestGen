package de.hpi.asg.breezetestgen.domain.components

import de.hpi.asg.breezetestgen.Loggable
import de.hpi.asg.breezetestgen.domain.{PullChannel, PushChannel, _}

object BrzComponent {
  type SyncSpec = Channel.Spec[SyncChannel[Channel.Endpoint]]
  type PullSpec = Channel.Spec[PullChannel[Channel.Endpoint]]
  type PushSpec = Channel.Spec[PushChannel[Channel.Endpoint]]
}

/** base class for Breeze components
  *
  * A Breeze handshake component with its wiring is represented by subclasses of this.
  * During a session, only one instance of this will be created per component in the netlist.
  * However, the components behaviour could be instantiated using multiple different states.
  *
  */
abstract class BrzComponent(val id: HandshakeComponent.Id) extends HandshakeComponent with Loggable {
  type Behaviour <: BrzComponentBehaviour[_, _]
  type C
  type D

  def behaviour(option: Option[Component.State[C, D]]): Behaviour
}
