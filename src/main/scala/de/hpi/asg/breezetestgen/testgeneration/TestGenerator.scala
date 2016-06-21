package de.hpi.asg.breezetestgen.testgeneration

import de.hpi.asg.breezetestgen.Loggable
import de.hpi.asg.breezetestgen.actors.ComponentActor.Decision
import de.hpi.asg.breezetestgen.actors.HandshakeActor
import de.hpi.asg.breezetestgen.domain.components.BrzComponentBehaviour.{DecisionPossibilities, NormalFlowReaction}
import de.hpi.asg.breezetestgen.domain.components.HandshakeComponent
import de.hpi.asg.breezetestgen.domain.{Netlist, Signal, SignalFromPassive}
import de.hpi.asg.breezetestgen.testgeneration.constraintsolving.ConstraintVariable
import de.hpi.asg.breezetestgen.testing.TestEvent
import de.hpi.asg.breezetestgen.testing.coverage.ChannelActivationCoverage

class TestGenerator(private val netlist: Netlist) extends Loggable {
  import TestGenerator._

  val collectedTests = new CollectedTests(ChannelActivationCoverage.forNetlist(netlist))
  val informationHubs = scala.collection.mutable.Map.empty[Netlist.Id, InformationHub]
  val waitingForState = scala.collection.mutable.Map.empty[Netlist.Id, WaitingForState]
  val backlog = scala.collection.mutable.Map.empty[Netlist.Id, (SleepingExecution, Netlist.State)]

  def start(): List[TestGenerationAction] = {
    info("Starting test generation")
    val runId = nextRunId()
    val informationHub = InformationHub.forNetlist(runId, netlist)
    informationHubs += runId -> informationHub

    List(
      CreateMainNetlist(runId, None),
      SendToMainNetlist(runId, informationHub.initialRequests())
    )
  }

  def onPortSignal(implicit runId: Netlist.Id, signal: Signal, testEvent: TestEvent): List[TestGenerationAction] = {
    info(s"Got signal from MainNetlist: $signal")
    val informationHub = informationHubs(runId)
    informationHub.handlePortSignal(signal, testEvent) match {
      case Left(signals) => List(SendToMainNetlist(runId, signals))
      case Right(generatedTestO) => testFinished(runId, generatedTestO)
    }
  }

  // in contrast to the other handlers, this one just returns the result directly. cause it's such a simple use case
  def onNormalFlow(implicit runId: Netlist.Id, nf: NormalFlowReaction): TestEvent = {
    trace("recording normalFlowReaction")
    informationHub.handleReaction(nf)
  }

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

  private def testFinished(runId: Netlist.Id, generatedTestO: Option[GeneratedTest]): List[TestGenerationAction] = {
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

  private def runIsOver(runId: Netlist.Id): List[TestGenerationAction] = {
    if(backlog.nonEmpty) {
      //TODO decide more intelligent here
      val id = backlog.keySet.head
      StopMainNetlist(runId) :: resumeTest(id)
    } else if(informationHubs.nonEmpty)
      StopMainNetlist(runId) :: Nil
    else
      stop(AbortGeneration)
  }

  private def resumeTest(runId: Netlist.Id): List[TestGenerationAction] = {
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


  case class SleepingExecution(infoHubState: InformationHub.State, decision: Decision)
  case class WaitingForState(possibilities: DecisionPossibilities,
                             sleepingExecutions: Map[ConstraintVariable,SleepingExecution])

  private sealed trait StopReason
  private case object Done extends StopReason
  private case object AbortGeneration extends StopReason
  private case object GenerationProblem extends StopReason

  private[this] var curRunId = -1
  private def nextRunId(): Netlist.Id = {
    val r = curRunId
    curRunId -= 1
    r
  }
}
