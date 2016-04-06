package de.hpi.asg.breezetestgen.testgeneration

import de.hpi.asg.breezetestgen.constraintsolving.ConstraintCollection
import de.hpi.asg.breezetestgen.domain.components.HandshakeComponent.Reaction
import de.hpi.asg.breezetestgen.testing.{IOEvent, TestEvent}

/** Central place for gathering information during a test/simulation run.
  *
  * Gathers [[de.hpi.asg.breezetestgen.testing.TestEvent]]s in a [[TestBuilder]],
  * a [[de.hpi.asg.breezetestgen.constraintsolving.ConstraintCollection]] and (later) coverage statistics.
  *
  */
class InformationHub(parentCollection: ConstraintCollection, testBuilder: TestBuilder) {
  var cc: ConstraintCollection = parentCollection.fork()

  /** records reaction from [[de.hpi.asg.breezetestgen.domain.components.BrzComponentBehaviour]]
    *
    * @param reaction reaction from handshake component
    * @return a [[TestEvent]] for further building of tests, if a [[TestOp]] was specified
    */
  def handleReaction(reaction: Reaction): Option[TestEvent] = {
    cc = cc.add(reaction.constraintVariables)

    // TODO: extract coverage info from signals

    reaction.testOp.map {
      case Merge(te) => testBuilder.merge(te)
      case AddIOEvent(te, s) => testBuilder.addSuccessor(te, IOEvent(s))
    }
  }

  /** returns the current state, maybe used for duplication or such things later */
  def state = (cc, testBuilder)
}
