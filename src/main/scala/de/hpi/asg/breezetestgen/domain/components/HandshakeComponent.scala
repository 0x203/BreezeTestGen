package de.hpi.asg.breezetestgen.domain.components

import de.hpi.asg.breezetestgen.constraintsolving.ConstraintVariable
import de.hpi.asg.breezetestgen.domain.Signal
import de.hpi.asg.breezetestgen.testgeneration.TestOp

object HandshakeComponent {
  type Id = Int

  case class State[C, D](controlState: C, dataState: D)

  case class Reaction(signals: Set[Signal], testOp: Option[TestOp], constraintVariables: Set[ConstraintVariable]) {
    def addSignal(s: Signal): Reaction = copy(signals = signals + s)
    def setTestOp(op: TestOp):Reaction = copy(testOp = Option(op))
    def addConstraint(cv: ConstraintVariable): Reaction = copy(constraintVariables = constraintVariables + cv)
    def addConstraints(new_cvs: Traversable[ConstraintVariable]): Reaction =
      copy(constraintVariables = constraintVariables ++ new_cvs)
  }
  object Reaction {
    def empty: Reaction = Reaction(Set.empty, None, Set.empty)
  }
}

trait HandshakeComponent {
  def id: HandshakeComponent.Id
}
