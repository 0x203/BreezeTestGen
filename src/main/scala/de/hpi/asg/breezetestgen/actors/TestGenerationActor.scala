package de.hpi.asg.breezetestgen.actors

import akka.actor.{Actor, ActorRef}
import de.hpi.asg.breezetestgen.Loggable
import de.hpi.asg.breezetestgen.domain._
import de.hpi.asg.breezetestgen.domain.components.BrzComponentBehaviour._
import de.hpi.asg.breezetestgen.testgeneration.{InformationHub, VariableData}
import de.hpi.asg.breezetestgen.testgeneration.constraintsolving._
import de.hpi.asg.breezetestgen.actors.ComponentActor.Decision
import de.hpi.asg.breezetestgen.testing.{IOEvent, TestEvent}

import scala.collection.mutable

object TestGenerationActor {
  case object Start

  private sealed trait StopReason
  private case object Done extends StopReason
  private case object Error extends StopReason

  case class ResumableRun(infoHubState: InformationHub.State, decision: Decision, inquirer: ActorRef)
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

    case HandshakeActor.Signal(runId, ds, testEvent) =>
      info(s"Got signal from MainNetlist: $ds")
      val informationHub = running._1
      val successor = informationHub.newIOEvent(ds, testEvent)
      if(ds == Acknowledge(1))  //stop for other reasons?!
        stop(Done)
      else
        mirrorSignal(runId, ds, successor)

    case nf: NormalFlowReaction =>
      trace("recording normalFlowReaction")
      val informationHub = running._1
      // reply with TestEvent
      sender() ! informationHub.handleReaction(nf)
    case DecisionRequired(possibilities) =>
      info(s"DecisionRequired: ${possibilities.keys}")
      val ccs = createFeasibleCCs(possibilities)

      if (ccs.isEmpty) throw new RuntimeException("Cannot handle this yet.")

      val inquirer = sender()
      val (_, testBuilder, coverage) = running._1.state()


      val resumables: Map[ConstraintVariable, ResumableRun] = possibilities.withFilter(ccs.keySet contains _._1).map{case (cv, (reaction, newState)) =>
        val newInformationHub = new InformationHub(ccs(cv), testBuilder, coverage)
        val testEventO = newInformationHub.handleReaction(reaction)
        val decision = Decision(newState, reaction.signals, testEventO)
        cv -> ResumableRun(newInformationHub.state(), decision, inquirer)
      }


      //TODO: instead of deciding one could copy at this point
      val decisionCV = decide(possibilities.filterKeys(ccs.keySet contains _))

      // add others to backlog
      backlog ++= (resumables - decisionCV).map{ case (_, r) =>
        val i = nextRunId
        nextRunId -= 1
        i -> r
      }

      // resume the chosen one
      val resumable = resumables(decisionCV)
      running = (InformationHub.fromState(resumable.infoHubState), running._2)
      resumable.inquirer ! resumable.decision
  }


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
      .map{ds => HandshakeActor.Signal(runId, ds, teO getOrElse IOEvent(ds))} // create understandable signals
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

  private def stop(reason: StopReason) = {
    info(s"I'm stopping because of: $reason")
    val informationHub = running._1

    val (cc, tb, coverage) = informationHub.state()
    info(s"Current ConstraintCollection: $cc")
    TestInstantiator.random(cc, tb) match {
      case Some(test) =>
        import de.hpi.asg.breezetestgen.testing.JsonFromTo
        info(s"here is a test, anyway: ${JsonFromTo.toJson(test)}")
        info(s"Coverage: ${coverage.percentageCovered}")
      case None => info("Not even found a test.")
    }

    // TODO: give feedback to somebody
    //context.stop(self)
    context.system.terminate()
  }

}
