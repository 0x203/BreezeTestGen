package de.hpi.asg.breezetestgen.actors

import akka.actor.{Actor, ActorRef}
import de.hpi.asg.breezetestgen.Loggable
import de.hpi.asg.breezetestgen.domain._
import de.hpi.asg.breezetestgen.testgeneration.{InformationHub, VariableData}
import de.hpi.asg.breezetestgen.testgeneration.constraintsolving._
import de.hpi.asg.breezetestgen.actors.ComponentActor.Decision
import de.hpi.asg.breezetestgen.actors.HandshakeActor.{GetState, MyState}
import components.BrzComponentBehaviour.{DecisionPossibilities, DecisionRequired, NormalFlowReaction}
import de.hpi.asg.breezetestgen.testing.coverage.Coverage
import de.hpi.asg.breezetestgen.testing.{IOEvent, Test, TestEvent}

import scala.collection.mutable

object TestGenerationActor {
  case object Start

  private sealed trait StopReason
  private case class Done(tests: Set[GeneratedTest]) extends StopReason
  private case object Error extends StopReason

  case class GeneratedTest(test: Test, coverage: Coverage)

  case class ResumableRun(infoHubState: InformationHub.State, decision: Decision, netlistState: Option[Netlist.State])
}

class TestGenerationActor(protected val netlist: Netlist) extends Actor with MainNetlistCreator with Loggable {
  import TestGenerationActor._

  def receive = {
    case Start =>
      info("Starting test generation")
      val runId = nextRunId; nextRunId -= 1

      val inits = initialRequests(runId)

      val informationHub = InformationHub.fromInitialSignals(netlist, inits)
      val netlistActor = newNetlistActor(runId, None, Some(self))
      running = (informationHub, netlistActor)

      // send inits out; activate (lowest chanID) first
      sendOutSignals(runId, inits.toSeq.sortBy(_.channelId))

    case HandshakeActor.Signal(runId :: _, ds, testEvent) =>
      info(s"Got signal from MainNetlist: $ds")
      val informationHub = running._1
      val successor = informationHub.newIOEvent(ds, testEvent)
      if(ds == Acknowledge(1))  //stop for other reasons?!
        testFinished()
      else
        mirrorSignal(runId, ds, successor)

    case nf: NormalFlowReaction =>
      trace("recording normalFlowReaction")
      val informationHub = running._1
      // reply with TestEvent
      sender() ! informationHub.handleReaction(nf)

    case HandshakeActor.DecisionRequired(idChain, componentId, DecisionRequired(possibilities)) =>
      info(s"DecisionRequired: ${possibilities.keys}")
      val ccs = createFeasibleCCs(possibilities)

      if (ccs.isEmpty) throw new RuntimeException("Cannot handle this yet.")

      val (infoHub, mainNetlistActor) = running
      val (_, testBuilder, coverage) = infoHub.state()

      mainNetlistActor ! GetState

      val resumables: Map[ConstraintVariable, ResumableRun] = possibilities.withFilter(ccs.keySet contains _._1).map{case (cv, (reaction, newState)) =>
        val newInformationHub = new InformationHub(ccs(cv), testBuilder, coverage)
        val testEventO = newInformationHub.handleReaction(reaction)
        val decision = Decision(idChain.tail, componentId, newState, reaction.signals, testEventO)
        cv -> ResumableRun(newInformationHub.state(), decision, None)
      }

      info(s"created ${resumables.size} resumables, will decide for one")

      //TODO: instead of deciding one could copy at this point
      val decisionCV = decide(possibilities.filterKeys(ccs.keySet contains _))

      // add others to backlog
      val others = (resumables - decisionCV).map{ case (_, r) =>
        val i = nextRunId
        nextRunId -= 1
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
      val netlistActor = running._2
      running = (InformationHub.fromState(resumable.infoHubState), netlistActor)

      info("send decision to inquirer")
      netlistActor ! resumable.decision
  }

  private var resumable2BContinued: ResumableRun = _
  private var waitingForState: Set[Netlist.Id] = _
  private val backlog = mutable.Map.empty[Netlist.Id, ResumableRun]
  private var running : (InformationHub, ActorRef) = _

  private var nextRunId: Int = -1

  private def signalOnPort(port: Port): Signal = port match {
    case sp: SyncPort => sp.createSignal()
    case dp: DataPort =>
      val v = new Variable(dp.name, dp.bitCount, dp.isSigned)
      val d = new VariableData(v, null)
      dp.createSignal(d)
  }

  private def initialRequests(runId: Netlist.Id): Set[Signal] = {
    netlist.activePorts.map(signalOnPort)
  }

  private def sendOutSignals(runId: Netlist.Id, signals: Traversable[Signal], teO: Option[TestEvent] = None) = {
    signals
      .map{ds => HandshakeActor.Signal(runId :: Nil, ds, teO getOrElse IOEvent(ds))} // create understandable signals
      .foreach(running._2 ! _)  // send them out
  }

  private def createFeasibleCCs(possibilities: DecisionPossibilities): Map[ConstraintVariable, ConstraintCollection] =
    possibilities.keys
      .map{case constraint => constraint -> running._1.cc.fork().add(List(constraint))}
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
    val informationHub = running._1

    val followerEvent = informationHub.newIOEvent(answerSignal, testEvent)
    sendOutSignals(nlId, Option(answerSignal), Option(followerEvent))
  }

  private var gencount = 5
  private var testsSoFar = Set.empty[GeneratedTest]

  private def testFinished(): Unit = {
    info("A Test finished.")
    val (informationHub, netlistActor) = running
    context.stop(netlistActor)

    val (cc, tb, coverage) = informationHub.state()
    info(s"Current ConstraintCollection: $cc")
    TestInstantiator.random(cc, tb) match {
      case Some(test) =>
        val generated = GeneratedTest(test, coverage)
        testsSoFar += generated

        import de.hpi.asg.breezetestgen.testing.JsonFromTo
        info(s"here is a test, anyway: ${JsonFromTo.toJson(test)}")
        info(s"Coverage: ${coverage.percentageCovered}")

        stop(Done(Set(generated)))
      case None => info("Not even found a test.")
    }

    if (backlog.isEmpty) {
      stop(Done(testsSoFar))
    } else if(gencount == 0) {
      stop(Error)
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

    running = (newInfoHub, netlistActor)

    netlistActor ! resumableRun.decision
  }

  private def stop(reason: StopReason) = {
    reason match {
      case Done(tests) =>
        import de.hpi.asg.breezetestgen.testing.JsonFromTo
        info(s"I'm done with generating ${tests.size} tests:")
        for(genTest <- tests) {
          info(s"${genTest.coverage.percentageCovered}% with: ${JsonFromTo.toJson(genTest.test)}")
        }

        //TODO: merge coverages, find minimal suite

      case Error =>
        //TODO: use better distinction of reasons here
        info("generated some tests ithout hitting maximum coverage")

    }

    // TODO: give feedback to somebody
    //context.stop(self)
    context.system.terminate()
  }

}
