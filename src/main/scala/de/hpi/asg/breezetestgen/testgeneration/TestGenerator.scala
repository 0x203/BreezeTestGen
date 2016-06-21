package de.hpi.asg.breezetestgen.testgeneration

import de.hpi.asg.breezetestgen.Loggable
import de.hpi.asg.breezetestgen.actors.ComponentActor.Decision
import de.hpi.asg.breezetestgen.actors.HandshakeActor
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
    info("Starting test generation")

    val informationHub = InformationHub.forNetlist(runId, netlist)
    informationHubs += runId -> informationHub

    List(
      CreateMainNetlist(runId, None),
      SendToMainNetlist(runId, informationHub.initialRequests())
    )
  }

  def onPortSignal(runId: Netlist.Id, signal: Signal, testEvent: TestEvent): List[TestGenerationAction] = {
    info(s"Got signal from MainNetlist: $signal")
    val informationHub = informationHubs(runId)
    informationHub.handlePortSignal(signal, testEvent) match {
      case Left(signals) => List(SendToMainNetlist(runId, signals))
      case Right(generatedTestO) => testFinished(runId, generatedTestO)
    }
  }

  // in contrast to the other handlers, this one just returns the result directly. cause it's such a simple use case
  def onNormalFlow(runId: Netlist.Id, nf: NormalFlowReaction): TestEvent = {
    trace("recording normalFlowReaction")
    informationHub.handleReaction(nf)
  }

  def onDecisionRequired(runId: Netlist.Id, possibilities: DecisionPossibilities): List[TestGenerationAction] = {
    List()
  }

  def onWholeState(runId: Netlist.Id, state: Netlist.State): List[TestGenerationAction] = {
    List()
  }

  private def testFinished(runId: Netlist.Id, generatedTestO: Option[GeneratedTest]): List[TestGenerationAction] = {
    info(s"Test $runId finished.")

    generatedTestO match {
      case Some(generatedTest) => collectedTests.foundNewTest(generatedTest)
      case None                => info("Not even found a test.")
    }

    if(collectedTests.combinedCoverage.isComplete) {
      stop(Done)
    } else if(backlog.isEmpty) {
      stop(AbortGeneration)
    } else {
      //TODO decide more intelligent here
      val (id, resumable) = backlog.head
      StopMainNetlist(runId) :: resumeTest(id, resumable)
    }
  }

  private def resumeTest(runId: Netlist.Id, sleepingExecution: SleepingExecution): List[TestGenerationAction] = {
    info(s"resuming test with id $runId")

    val newInfoHub = InformationHub.fromState(runId, netlist, sleepingExecution.infoHubState)
    info(s"resumed infohub-state: ${newInfoHub.state()}")
    info(s"resumed netlistState: ${sleepingExecution.netlistState}")

    backlog.remove(runId)
    informationHubs += runId -> newInfoHub

    List(
      CreateMainNetlist(runId, Some(sleepingExecution.netlistState)),
      SendDecision(runId, sleepingExecution.decision)
    )
  }

  private def stop(reason: StopReason) = {
    val result = reason match {
      case Done =>
        val tests = collectedTests.testCollection
        info(s"Reached complete coverage with ${tests.size} tests.")
        CompleteCoverage(tests)

      case AbortGeneration =>
        val testsSoFar = collectedTests.testCollection
        info(s"Aborting test generation having ${testsSoFar.size} tests until generated.")
        PartialCoverage(testsSoFar)

      case GenerationProblem =>
        info("Somewhere a problem occurred.")
        GenerationError("something went wrong")
    }

    // stop all running netlists and send result afterwards
    informationHubs.keys.map(StopMainNetlist(_)).toList :+ FinishedGeneration(result)
  }

  implicit private def informationHub(implicit runId: Netlist.Id) = informationHubs(runId)
}

object TestGenerator {

  sealed trait TestGenerationAction
  case class CreateMainNetlist(runId: Netlist.Id, stateO: Option[Netlist.State]) extends TestGenerationAction
  case class StopMainNetlist(runId: Netlist.Id) extends TestGenerationAction
  case class SendToMainNetlist(runId: Netlist.Id, signals: Seq[HandshakeActor.Signal]) extends TestGenerationAction
  case class RequestWholeState(runId: Netlist.Id) extends TestGenerationAction
  case class SendDecision(runId: Netlist.Id, decision: Decision) extends TestGenerationAction
  case class FinishedGeneration(generationResult: GenerationResult) extends TestGenerationAction


  case class SleepingExecution(infoHubState: InformationHub.State, netlistState: Netlist.State, decision: Decision)

  private sealed trait StopReason
  private case object Done extends StopReason
  private case object AbortGeneration extends StopReason
  private case object GenerationProblem extends StopReason
}
