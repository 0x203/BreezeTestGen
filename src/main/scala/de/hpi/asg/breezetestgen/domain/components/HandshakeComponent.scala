package de.hpi.asg.breezetestgen.domain.components

import de.hpi.asg.breezetestgen.constraintsolving.ConstraintVariable
import de.hpi.asg.breezetestgen.domain.Signal
import de.hpi.asg.breezetestgen.testgeneration.TestOp

object HandshakeComponent {
  type Id = Int

  case class State[C, D](controlState: C, dataState: D)
}

trait HandshakeComponent {
  def id: HandshakeComponent.Id
}
