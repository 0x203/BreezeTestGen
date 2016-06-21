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

    val ids: Map[ConstraintVariable, Netlist.Id] = wfs.possibilities.map{case (cv, _) => cv -> nextRunId()}

    // add all to backlog
    backlog ++= wfs.sleepingExecutions.map{ case (cv, r) => ids(cv) -> (r, state) }

    // decide for some possibilities and resume them all
    decide(wfs.possibilities)
      .map(ids)
      .map(resumeTest)
      .fold(StopMainNetlist(runId) :: Nil)(_ ++ _)
  }

  private def decide(possibilities: DecisionPossibilities): Set[ConstraintVariable] = {
    possibilities.keySet
  }
}
