package de.hpi.asg.breezetestgen.testgeneration

import de.hpi.asg.breezetestgen.Loggable
import de.hpi.asg.breezetestgen.domain.{Netlist, SignalFromPassive}
import de.hpi.asg.breezetestgen.domain.components.BrzComponentBehaviour.DecisionPossibilities
import de.hpi.asg.breezetestgen.domain.components.HandshakeComponent
import de.hpi.asg.breezetestgen.testgeneration.TestGenerator._
import de.hpi.asg.breezetestgen.testgeneration.constraintsolving.ConstraintVariable

/** Responsible for making decisions for appropriate components, i.e. Case.
  */
trait Decider extends Loggable {
  self: TestGenerator =>

  def onDecisionRequired(implicit runId: Netlist.Id,
                         idChain: List[Netlist.Id],
                         componentId: HandshakeComponent.Id,
                         possibilities: DecisionPossibilities): List[TestGenerationAction] = {
    info(s"DecisionRequired: ${possibilities.keys}")
    val sleepingExecutions = informationHubs(runId).createSleepingExecutions(idChain, componentId, possibilities)

    info(s"created ${sleepingExecutions.size} sleeping executions")

    if (sleepingExecutions.isEmpty)
      return runIsOver(runId)

    waitingForState += runId -> WaitingForState(possibilities.filterKeys(sleepingExecutions.contains), sleepingExecutions)

    List(RequestWholeState(runId))
  }

  def onWholeState(implicit runId: Netlist.Id, state: Netlist.State): List[TestGenerationAction] = {
    info(s"Got state of main netlist:$runId: $state")
    val wfs = waitingForState.remove(runId).get

    //TODO: instead of deciding one could copy at this point
    val decisionCV = decide(wfs.possibilities)

    // add others to backlog
    val others = (wfs.sleepingExecutions - decisionCV).map{ case (_, r) =>
      val i = nextRunId()
      i -> (r, state)
    }
    backlog ++= others

    // resume selected one in current run
    val toBeResumed = wfs.sleepingExecutions(decisionCV)
    informationHubs.update(runId, InformationHub.fromState(runId, netlist, toBeResumed.infoHubState))

    List(SendDecision(runId, toBeResumed.decision))
  }

  private def decide(possibilities: DecisionPossibilities): ConstraintVariable = {
    possibilities.mapValues(_._1).toSeq.sortBy(tpl => {
      // sort ascending by SignalFromPassive and choose last
      // this should give us the possibility with most acknowledges, which should lead to an early finish
      tpl._2.signals.count(_.isInstanceOf[SignalFromPassive])
    }).last._1
  }
}
