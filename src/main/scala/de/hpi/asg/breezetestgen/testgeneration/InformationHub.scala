package de.hpi.asg.breezetestgen.testgeneration

import de.hpi.asg.breezetestgen.constraintsolving.ConstraintCollection
import de.hpi.asg.breezetestgen.domain.components.ComponentBehaviour
import ComponentBehaviour.Reaction
import de.hpi.asg.breezetestgen.testing.TestEvent

/** Central place for gathering information during a test/simulation run.
  *
  * Gathers [[de.hpi.asg.breezetestgen.testing.TestEvent]]s in a [[TestBuilder]],
  * a [[de.hpi.asg.breezetestgen.constraintsolving.ConstraintCollection]] and (later) coverage statistics.
  *
  */
class InformationHub(parentCollection: ConstraintCollection, testBuilder: TestBuilder) {
  var cc: ConstraintCollection = parentCollection.fork()

  /** records reaction from [[ComponentBehaviour]]
    *
    * @param reaction reaction from handshake component
    * @return a [[TestEvent]] for further building of tests, if a [[TestOp]] was specified
    */
  def handleReaction(reaction: Reaction): Option[TestEvent] = {
    cc = cc.add(reaction.constraintVariables)

    // TODO: extract coverage info from signals

    reaction.testOp.map {
      case Merge(te) => testBuilder.merge(te)
      case AddSyncEvent(te, p) => testBuilder.addSuccessor(te, p)
      // TODO: when subclasses of data are defined, differentiate between them here
      //case AddDataEvent(te, p, i) => testBuilder.addSuccessor(te, p, i)
    }
  }

  /** returns the current state, maybe used for duplication or such things later */
  def state = (cc, testBuilder)
}
