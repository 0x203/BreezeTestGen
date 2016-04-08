package de.hpi.asg.breezetestgen.domain.components

object HandshakeComponent {
  type Id = Int

  case class State[C, D](controlState: C, dataState: D)
}

trait HandshakeComponent {
  def id: HandshakeComponent.Id
}
