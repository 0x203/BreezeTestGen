package de.hpi.asg.breezetestgen.testgeneration

import constraintsolving.ConstraintCollection
import de.hpi.asg.breezetestgen.domain.{DataRequest, Signal}
import de.hpi.asg.breezetestgen.domain.components.BrzComponentBehaviour.NormalFlowReaction
import de.hpi.asg.breezetestgen.testing.{IOEvent, TestEvent}

/** Central place for gathering information during a test/simulation run.
  *
  * Gathers [[de.hpi.asg.breezetestgen.testing.TestEvent]]s in a [[TestBuilder]],
  * a [[constraintsolving.ConstraintCollection]] and (later) coverage statistics.
  *
  */
class InformationHub(var cc: ConstraintCollection, testBuilder: TestBuilder) {

  /** records reaction from [[de.hpi.asg.breezetestgen.domain.components.BrzComponentBehaviour]]
    *
    * @param reaction reaction from handshake component
    * @return a [[TestEvent]] for further building of tests, if a [[TestOp]] was specified
    */
  def handleReaction(reaction: NormalFlowReaction): Option[TestEvent] = {
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
object InformationHub {
  def fromInitialSignals(initialSignals: Set[Signal]): InformationHub = {
    new InformationHub(
      ConstraintCollection(
        variables = initialSignals.collect{case DataRequest(_, vd :VariableData) => vd.underlying}
      ),
      TestBuilder.withOrigins(
        initialSignals.map(IOEvent(_))
      )
    )
  }
}
