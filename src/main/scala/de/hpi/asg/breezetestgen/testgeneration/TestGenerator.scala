package de.hpi.asg.breezetestgen.testgeneration

import de.hpi.asg.breezetestgen.Loggable
import de.hpi.asg.breezetestgen.actors.ComponentActor.Decision
import de.hpi.asg.breezetestgen.domain.components.BrzComponentBehaviour.{DecisionPossibilities, NormalFlowReaction}
import de.hpi.asg.breezetestgen.domain.{Netlist, Signal}
import de.hpi.asg.breezetestgen.testing.TestEvent
import de.hpi.asg.breezetestgen.testing.coverage.ChannelActivationCoverage

class TestGenerator(private val netlist: Netlist) extends Loggable {
  import TestGenerator._

  val collectedTests = new CollectedTests(ChannelActivationCoverage.forNetlist(netlist))
  val informationHubs = scala.collection.mutable.Map.empty[Netlist.Id, InformationHub]
  val backlog = scala.collection.mutable.Map.empty[Netlist.Id, SleepingExecution]

  def start(runId: Netlist.Id): List[TestGenerationAction] = {
    List(CreateMainNetlist(runId))
  }

  def onPortSignal(runId: Netlist.Id, signal: Signal, te: TestEvent): List[TestGenerationAction] = {
    List()
  }

  def onNormalFlow(runId: Netlist.Id, nf: NormalFlowReaction): List[TestGenerationAction] = {
    List()
  }

  def onDecisionRequired(runId: Netlist.Id, possibilities: DecisionPossibilities): List[TestGenerationAction] = {
    List()
  }

  def onWholeState(runId: Netlist.Id, state: Netlist.State): List[TestGenerationAction] = {
    List()
  }
}

object TestGenerator {

  sealed trait TestGenerationAction
  case class CreateMainNetlist(runId: Netlist.Id) extends TestGenerationAction
  case class StopMainNetlist(runId: Netlist.Id) extends TestGenerationAction
  case class SendToMainNetlist(runId: Netlist.Id, signals: Seq[Signal]) extends TestGenerationAction
  case class RequestWholeState(runId: Netlist.Id) extends TestGenerationAction
  case class SendDecision(runId: Netlist.Id, decision: Decision) extends TestGenerationAction

  case class AnswerToComponent(testEvent: TestEvent)


  case class SleepingExecution(infoHubState: InformationHub.State)
}
