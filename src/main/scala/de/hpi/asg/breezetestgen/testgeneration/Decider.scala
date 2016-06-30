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

  def onDecisionRequired(implicit runId: Int,
                         componentId: HandshakeComponent.Id,
                         possibilities: DecisionPossibilities): List[TestGenerationAction] = {
    info(s"DecisionRequired: ${possibilities.keys}")
    val sleepingExecutions = informationHubs(runId).createSleepingExecutions(componentId, possibilities)

    info(s"created ${sleepingExecutions.size} sleeping executions")

    if (sleepingExecutions.isEmpty)
      return runIsOver(runId)

    waitingForState += runId -> WaitingForState(possibilities.filterKeys(sleepingExecutions.contains), sleepingExecutions)

    List(RequestWholeState(runId))
  }

  def onWholeState(implicit runId: Int, state: Netlist.State): List[TestGenerationAction] = {
    info(s"Got state of main netlist:$runId: $state")
    val wfs = waitingForState.remove(runId).get

    val ids: Map[ConstraintVariable, Int] = wfs.possibilities.map{case (cv, _) => cv -> nextRunId()}

    // add all to backlog
    backlog ++= wfs.sleepingExecutions.map{ case (cv, r) => ids(cv) -> (r, state) }

    // decide for some possibilities and resume them all
    decideForOne(wfs.possibilities)
      .map(ids)
      .map(resumeTest)
      .fold(StopMainNetlist(runId) :: Nil)(_ ++ _)
  }

  private def decideForOne(possibilities: DecisionPossibilities): Set[ConstraintVariable] = {
    val acknowledging = possibilities.mapValues(_._1).toSeq.sortBy(tpl => {
      // sort ascending by SignalFromPassive and choose last
      // this should give us the possibility with most acknowledges, which should lead to an early finish
      tpl._2.signals.count(_.isInstanceOf[SignalFromPassive])
    }).last._1
    Set(acknowledging)
  }

  private def takeAll(possibilities: DecisionPossibilities): Set[ConstraintVariable] = {
    possibilities.keySet
  }
}
