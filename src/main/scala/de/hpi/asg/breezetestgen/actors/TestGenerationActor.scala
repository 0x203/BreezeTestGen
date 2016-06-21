package de.hpi.asg.breezetestgen.actors

import akka.actor.{Actor, ActorRef}
import de.hpi.asg.breezetestgen.Loggable
import de.hpi.asg.breezetestgen.domain._
import de.hpi.asg.breezetestgen.testgeneration._
import de.hpi.asg.breezetestgen.testgeneration.constraintsolving._
import de.hpi.asg.breezetestgen.actors.ComponentActor.Decision
import de.hpi.asg.breezetestgen.actors.HandshakeActor.{GetState, MyState}
import components.BrzComponentBehaviour.{DecisionPossibilities, DecisionRequired, NormalFlowReaction}
import de.hpi.asg.breezetestgen.testing.coverage.ChannelActivationCoverage
import de.hpi.asg.breezetestgen.testing.{IOEvent, TestEvent}

import scala.collection.mutable

object TestGenerationActor {
  case object Start

  private sealed trait StopReason
  private case object Done extends StopReason
  private case object AbortGeneration extends StopReason
  private case object GenerationProblem extends StopReason

  case class ResumableRun(infoHubState: InformationHub.State, decision: Decision, netlistState: Option[Netlist.State])


  private def signalOnPort(port: Port): Signal = port match {
    case sp: SyncPort => sp.createSignal()
    case dp: DataPort =>
      val v = new Variable(dp.name, dp.bitCount, dp.isSigned)
      val d = new VariableData(v, null)
      dp.createSignal(d)
  }


  private[this] var curRunId = -1
  private def nextRunId(): Netlist.Id = {
    val r = curRunId
    curRunId -= 1
    r
  }

}

class TestGenerationActor(protected val netlist: Netlist) extends Actor with MainNetlistCreator with Loggable {
  import TestGenerationActor._

  var inquirer: ActorRef = _
  val collectedTests = new CollectedTests(ChannelActivationCoverage.forNetlist(netlist))

  def receive = {
    case Start =>
      info("Starting test generation")
      inquirer = sender()
      val runId = nextRunId()

      val inits = initialRequests(runId)

      val informationHub = InformationHub.fromInitialSignals(netlist, inits)
      val netlistActor = newNetlistActor(runId, None, Some(self))
      running += runId -> (informationHub, netlistActor)

      // send inits out; activate (lowest chanID) first
      sendOutSignals(runId, inits.toSeq.sortBy(_.channelId))

    case HandshakeActor.Signal(runId :: _, ds, testEvent) =>
      info(s"Got signal from MainNetlist: $ds")
      val informationHub = running(runId)._1
      val successor = informationHub.newIOEvent(ds, testEvent)
      if(ds == Acknowledge(1))  //stop for other reasons?!
        testFinished(runId)
      else
        mirrorSignal(runId, ds, successor)

    case HandshakeActor.NormalFlowReaction(runId :: _, nf: NormalFlowReaction) =>
      trace("recording normalFlowReaction")
      val informationHub = running(runId)._1
      // reply with TestEvent
      sender() ! informationHub.handleReaction(nf)

    case HandshakeActor.DecisionRequired(runId:: idChain, componentId, DecisionRequired(possibilities)) =>
      info(s"DecisionRequired: ${possibilities.keys}")
      val (infoHub, mainNetlistActor) = running(runId)
      val (currentCC, testBuilder, coverage) = infoHub.state()

      val ccs = createFeasibleCCs(currentCC, possibilities)

      if (ccs.isEmpty) throw new RuntimeException("Cannot handle this yet.")


      mainNetlistActor ! GetState

      val resumables: Map[ConstraintVariable, ResumableRun] = possibilities.withFilter(ccs.keySet contains _._1).map{case (cv, (reaction, newState)) =>
        val newInformationHub = new InformationHub(ccs(cv), testBuilder, coverage)
        val testEventO = newInformationHub.handleReaction(reaction)
        val decision = Decision(idChain, componentId, newState, reaction.signals, testEventO)
        cv -> ResumableRun(newInformationHub.state(), decision, None)
      }

      info(s"created ${resumables.size} resumables, will decide for one")

      //TODO: instead of deciding one could copy at this point
      val decisionCV = decide(possibilities.filterKeys(ccs.keySet contains _))

      // add others to backlog
      val others = (resumables - decisionCV).map{ case (_, r) =>
        val i = nextRunId()
        i -> r
      }
      waitingForState = others.keySet
      backlog ++= others

      resumable2BContinued = resumables(decisionCV)

    case MyState(runId :: _, _, state: Netlist.State) =>
      info(s"Got state of main netlist:$runId: $state")
      for(stateAwaiter <- waitingForState) {
        backlog.update(stateAwaiter, backlog(stateAwaiter).copy(netlistState = Option(state)))
      }

      // resume the chosen one
      val resumable = resumable2BContinued
      info("continue with resumable")
      val netlistActor = running(runId)._2
      running.update(runId, (InformationHub.fromState(resumable.infoHubState), netlistActor))

      info("send decision to inquirer")
      netlistActor ! resumable.decision
  }

  private var resumable2BContinued: ResumableRun = _
  private var waitingForState: Set[Netlist.Id] = _
  private val backlog = mutable.Map.empty[Netlist.Id, ResumableRun]
  private val running = mutable.Map.empty[Netlist.Id, (InformationHub, ActorRef)]

  private def initialRequests(runId: Netlist.Id): Set[Signal] = {
    netlist.activePorts.map(signalOnPort)
  }

  private def sendOutSignals(runId: Netlist.Id, signals: Traversable[Signal], teO: Option[TestEvent] = None) = {
    signals
      .map{ds => HandshakeActor.Signal(runId :: Nil, ds, teO getOrElse IOEvent(ds))} // create understandable signals
      .foreach(running(runId)._2 ! _)  // send them out
  }

  private def createFeasibleCCs(baseCC: ConstraintCollection, possibilities: DecisionPossibilities): Map[ConstraintVariable, ConstraintCollection] =
    possibilities.keys
      .map{case constraint => constraint -> baseCC.fork().add(List(constraint))}
      .filter{case (_, cc) => new ChocoSolver(cc).isFeasible}
      .toMap

  private def decide(possibilities: DecisionPossibilities): ConstraintVariable = {
    possibilities.mapValues(_._1).toSeq.sortBy(tpl => {
      // sort ascending by SignalFromPassive and choose last
      // this should give us the possibility with most acknowledges, which should lead to an early finish
      tpl._2.signals.count(_.isInstanceOf[SignalFromPassive])
    }).last._1
  }

  private val channelIdToPortId = portConnections.map(_.swap)

  /** reacts to a signal from the netlist with the counter-signal on the same port
    *
    * @param nlId netlistId
    * @param signal signal to be mirrored
    * @param testEvent predecessor of new one
    */
  private def mirrorSignal(nlId: Netlist.Id, signal: Signal, testEvent: TestEvent) = {
    val portId = channelIdToPortId(signal.channelId)
    val answerSignal = signalOnPort(netlist.ports(portId))
    val informationHub = running(nlId)._1

    val followerEvent = informationHub.newIOEvent(answerSignal, testEvent)
    sendOutSignals(nlId, Option(answerSignal), Option(followerEvent))
  }

  private var gencount = 5

  private def testFinished(runId: Netlist.Id): Unit = {
    info(s"Test $runId finished.")
    val (informationHub, netlistActor) = running(runId)
    context.stop(netlistActor)
    gencount -= 1

    val (cc, tb, coverage) = informationHub.state()
    info(s"Current ConstraintCollection: $cc")

    TestInstantiator.random(cc, tb) match {
      case Some(test) => collectedTests.foundNewTest(GeneratedTest(test, coverage))
      case None       => info("Not even found a test.")
    }

    if(collectedTests.combinedCoverage.isComplete) {
      stop(Done)
    } else if(gencount == 0 | backlog.isEmpty) {
      stop(AbortGeneration)
    } else {
      val (id, resumable) = backlog.head
      resumeTest(id, resumable)
    }
  }

  private def resumeTest(id: Netlist.Id, resumableRun: ResumableRun): Unit = {
    info(s"resuming test with id $id")
    backlog.remove(id)

    val newInfoHub = InformationHub.fromState(resumableRun.infoHubState)
    info(s"resumed infohub-state: ${newInfoHub.state()}")
    info(s"resumed netlistState: ${resumableRun.netlistState}")
    val netlistActor = newNetlistActor(id, resumableRun.netlistState, Some(self))

    running += id -> (newInfoHub, netlistActor)

    netlistActor ! resumableRun.decision
  }

  private def stop(reason: StopReason) = {
    reason match {
      case Done =>
        val tests = collectedTests.testCollection
        info(s"Reached complete coverage with ${tests.size} tests.")
        inquirer ! CompleteCoverage(tests)

      case AbortGeneration =>
        val testsSoFar = collectedTests.testCollection
        info(s"Aborting test generation having ${testsSoFar.size} tests until generated.")
        inquirer ! PartialCoverage(testsSoFar)

      case GenerationProblem =>
        info("Somewhere a problem occurred.")
        inquirer ! GenerationError("something went wrong")
    }

    context.stop(self)
  }

}
