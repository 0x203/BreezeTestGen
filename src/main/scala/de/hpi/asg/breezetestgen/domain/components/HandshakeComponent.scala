package de.hpi.asg.breezetestgen.domain.components

object HandshakeComponent {
  type Id = Int
}

trait HandshakeComponent {
  def id: HandshakeComponent.Id
}
