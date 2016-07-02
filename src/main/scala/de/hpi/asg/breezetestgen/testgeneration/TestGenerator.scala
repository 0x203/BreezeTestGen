package de.hpi.asg.breezetestgen.testgeneration

import de.hpi.asg.breezetestgen.actors.HandshakeActor
import de.hpi.asg.breezetestgen.actors.HandshakeActor.Decision
import de.hpi.asg.breezetestgen.domain.components.BrzComponentBehaviour.{DecisionPossibilities, NormalFlowReaction}
import de.hpi.asg.breezetestgen.domain.components.HandshakeComponent
import de.hpi.asg.breezetestgen.domain.{Netlist, Signal}
import de.hpi.asg.breezetestgen.testgeneration.constraintsolving.ConstraintVariable
import de.hpi.asg.breezetestgen.testing.TestEvent
import de.hpi.asg.breezetestgen.testing.coverage.ChannelActivationCoverage
import de.hpi.asg.breezetestgen.util.Loggable

class TestGenerator(val netlist: Netlist, maxLoopExecs: Int) extends Decider with Loggable {
  import TestGenerator._

  val collectedTests = new CollectedTests(ChannelActivationCoverage.forNetlist(netlist))
  val informationHubs = scala.collection.mutable.Map.empty[Int, InformationHub]
  val waitingForState = scala.collection.mutable.Map.empty[Int, WaitingForState]
  val backlog = scala.collection.mutable.Map.empty[Int, (SleepingExecution, Netlist.State)]

  info(s"Loops in this netlist: ${netlist.loopIds}")

  def start(): List[TestGenerationAction] = {
    info("Starting test generation")
    val runId = nextRunId()
    val informationHub = InformationHub.forNetlist(runId, netlist, maxLoopExecs)
    informationHubs += runId -> informationHub

    List(
      CreateMainNetlist(runId, None),
      SendToMainNetlist(runId, informationHub.initialRequests())
    )
  }

  def onPortSignal(implicit runId: Int, signal: Signal, testEvent: TestEvent): List[TestGenerationAction] = {
    info(s"Got signal from MainNetlist: $signal")
    val informationHub = informationHubs(runId)
    informationHub.handlePortSignal(signal, testEvent) match {
      case Left(signals) => List(SendToMainNetlist(runId, signals))
      case Right(generatedTestO) => testFinished(runId, generatedTestO)
    }
  }

  def onNormalFlow(implicit runId: Int,
                   compId: HandshakeComponent.Id,
                   nf: NormalFlowReaction): List[TestGenerationAction] = {
    trace("recording normalFlowReaction")
    informationHub.handleReaction(nf, compId) match {
      case Left(testEvent) => List(ReturnTestEvent(testEvent))
      case Right(generatedTestO) => testFinished(runId, generatedTestO)
    }
  }

  def onUnexpectedTermination(implicit runId: Int): List[TestGenerationAction] = {
    info(s"Actors of test $runId stopped unexpectedly.")
    runIsOver(runId)
  }

  private def testFinished(runId: Int, generatedTestO: Option[GeneratedTest]): List[TestGenerationAction] = {
    info(s"Test $runId finished.")

    generatedTestO match {
      case Some(generatedTest) => collectedTests.foundNewTest(generatedTest)
      case None                => info("Not even found a test.")
    }

    if(collectedTests.combinedCoverage.isComplete) {
      stop(Done)
    } else
      runIsOver(runId)
  }

  protected def runIsOver(runId: Int): List[TestGenerationAction] = {
    informationHubs -= runId
    warn(s"$runId is over")
    if(backlog.nonEmpty) {
      //TODO decide more intelligent here
      val id = backlog.keySet.head
      StopMainNetlist(runId) :: resumeTest(id)
    } else if(informationHubs.nonEmpty)
      StopMainNetlist(runId) :: Nil
    else
      stop(AbortGeneration)
  }

  protected def resumeTest(runId: Int): List[TestGenerationAction] = {
    info(s"resuming test with id $runId")
    val (sleepingExecution, netlistState) = backlog.remove(runId).get

    val newInfoHub = InformationHub.fromState(runId, netlist, sleepingExecution.infoHubState)
    info(s"resumed infohub-state: ${newInfoHub.state()}")
    info(s"resumed netlistState: $netlistState")

    informationHubs += runId -> newInfoHub

    List(
      CreateMainNetlist(runId, Some(netlistState)),
      SendDecision(runId, sleepingExecution.decision)
    )
  }

  private def stop(reason: StopReason) = {
    val result = reason match {
      case Done =>
        val tests = collectedTests.testCollection
        info(s"Reached complete coverage with ${tests.size} tests.")
        CompleteCoverage(netlist, tests)

      case AbortGeneration =>
        val testsSoFar = collectedTests.testCollection
        info(s"Aborting test generation having ${testsSoFar.size} tests generated so far.")
        PartialCoverage(netlist, testsSoFar)

      case GenerationProblem =>
        info("Somewhere a problem occurred.")
        GenerationError("something went wrong")
    }

    // stop all running netlists and send result afterwards
    informationHubs.keys.map(StopMainNetlist(_)).toList :+ FinishedGeneration(result)
  }

  implicit private def informationHub(implicit runId: Int) = informationHubs(runId)
}

object TestGenerator {

  sealed trait TestGenerationAction
  case class CreateMainNetlist(runId: Int, stateO: Option[Netlist.State]) extends TestGenerationAction
  case class StopMainNetlist(runId: Int) extends TestGenerationAction
  case class SendToMainNetlist(runId: Int, signals: Seq[HandshakeActor.Signal]) extends TestGenerationAction
  case class RequestWholeState(runId: Int) extends TestGenerationAction
  case class ReturnTestEvent(testEvent: TestEvent) extends TestGenerationAction
  case class SendDecision(runId: Int, decision: Decision) extends TestGenerationAction
  case class FinishedGeneration(generationResult: GenerationResult) extends TestGenerationAction


  case class SleepingExecution(infoHubState: InformationHub.State, decision: Decision)
  case class WaitingForState(possibilities: DecisionPossibilities,
                             sleepingExecutions: Map[ConstraintVariable,SleepingExecution])

  private sealed trait StopReason
  private case object Done extends StopReason
  private case object AbortGeneration extends StopReason
  private case object GenerationProblem extends StopReason

  private[this] var curRunId = 1
  def nextRunId(): Int = {
    val r = curRunId
    curRunId += 1
    r
  }
}
