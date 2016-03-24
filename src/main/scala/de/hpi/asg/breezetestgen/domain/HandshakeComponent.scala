package de.hpi.asg.breezetestgen.domain

object HandshakeComponent {
  type Id = Int
}

trait HandshakeComponent {
  def id: HandshakeComponent.Id
}
