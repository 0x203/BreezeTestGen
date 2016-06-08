package de.hpi.asg.breezetestgen.testgeneration

import constraintsolving.ConstraintCollection
import de.hpi.asg.breezetestgen.domain.{DataRequest, Netlist, Signal, SignalWithData}
import de.hpi.asg.breezetestgen.domain.components.BrzComponentBehaviour.NormalFlowReaction
import de.hpi.asg.breezetestgen.testing.coverage.{ChannelActivationCoverage, Coverage}
import de.hpi.asg.breezetestgen.testing.{IOEvent, TestEvent}

/** Central place for gathering information during a test/simulation run.
  *
  * Gathers [[de.hpi.asg.breezetestgen.testing.TestEvent]]s in a [[TestBuilder]],
  * a [[constraintsolving.ConstraintCollection]] and (later) coverage statistics.
  *
  */
class InformationHub(var cc: ConstraintCollection, testBuilder: TestBuilder, var coverage: Coverage) {

  /** records reaction from [[de.hpi.asg.breezetestgen.domain.components.BrzComponentBehaviour]]
    *
    * @param reaction reaction from handshake component
    * @return a [[TestEvent]] for further building of tests, if a [[TestOp]] was specified
    */
  def handleReaction(reaction: NormalFlowReaction): TestEvent = {
    //cc = cc.add(reaction.constraintVariables)
    reaction.signals
      .collect{case dataSignal: SignalWithData => dataSignal.data}
      .collect{case vd: VariableData => vd}
      .foreach(vd => {
        // TODO refactor this badly
        cc = cc.addVariable(vd.underlying)
        val cons = vd.constraint
        if (cons != null)
          cc = cc.addConstraint(cons)
      })

    coverage = coverage.withSignals(reaction.signals)

    reaction.testOp match {
      case Merge(te) => testBuilder.merge(te)
      case Follow(te) => te
    }
  }

  /** records a signal as an IOEvent
    *
    * @param signal the signal of the IOEvent
    * @param testEvent  the predecessor
    * @return the freshly created event for further tracking
    */
  def newIOEvent(signal: Signal, testEvent: TestEvent): TestEvent ={
    coverage = coverage.withSignal(signal)
    testBuilder.addSuccessor(testEvent, IOEvent(signal))
  }

  /** returns the current state, maybe used for duplication or such things later */
  def state(): InformationHub.State = (cc, testBuilder.duplicate(), coverage)
}
object InformationHub {
  type State = (ConstraintCollection, TestBuilder, Coverage)

  def fromState(state: State): InformationHub =
    new InformationHub(state._1, state._2, state._3)

  def fromInitialSignals(netlist: Netlist, initialSignals: Set[Signal]): InformationHub = {
    new InformationHub(
      ConstraintCollection(
        variables = initialSignals.collect{case DataRequest(_, vd :VariableData) => vd.underlying}
      ),
      TestBuilder.withOrigins(
        initialSignals.map(IOEvent(_))
      ),
      ChannelActivationCoverage.forNetlist(netlist).withSignals(initialSignals)
    )
  }
}
