package de.hpi.asg.breezetestgen.testgeneration

import de.hpi.asg.breezetestgen.constraintsolving.ConstraintCollection
import de.hpi.asg.breezetestgen.domain.ComponentBehaviour.Reaction

/** Central place for gathering information during a test/simulation run.
  *
  * Gathers [[de.hpi.asg.breezetestgen.testing.TestEvent]]s in a [[TestBuilder]],
  * a [[de.hpi.asg.breezetestgen.constraintsolving.ConstraintCollection]] and (later) coverage statistics.
  *
  */
class InformationHub(parentCollection: ConstraintCollection, testBuilder: TestBuilder) {
  var cc: ConstraintCollection = parentCollection.fork()

  def handleReaction(reaction: Reaction): Unit = {
    // cc = cc.addConstraints(reaction.cvs)
    // call testbuilder according to testop
    // extract coverage info from signals
  }

  def state = (cc, testBuilder)
}
