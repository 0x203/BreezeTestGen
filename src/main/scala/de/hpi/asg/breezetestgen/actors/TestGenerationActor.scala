package de.hpi.asg.breezetestgen.actors

import akka.actor.{Actor, ActorRef}
import de.hpi.asg.breezetestgen.Loggable
import de.hpi.asg.breezetestgen.domain._
import de.hpi.asg.breezetestgen.domain.components.BrzComponentBehaviour._
import de.hpi.asg.breezetestgen.testgeneration.{InformationHub, TestBuilder, VariableData, constraintsolving}
import constraintsolving.{ChocoSolver, ConstraintCollection, ConstraintVariable, Variable}
import de.hpi.asg.breezetestgen.actors.ComponentActor.Decision
import de.hpi.asg.breezetestgen.testing.{IOEvent, Test, TestEvent}

object TestGenerationActor {
  case object Start

  private sealed trait StopReason
  private case object Done extends StopReason
  private case object Error extends StopReason
}

class TestGenerationActor(protected val netlist: Netlist) extends Actor with MainNetlistCreator with Loggable {
  import TestGenerationActor._

  def receive = {
    case Start =>
      info("Starting test generation")
      val runId = nextRunId; nextRunId -= 1

      val inits = initialRequests(runId)

      informationHub = InformationHub.fromInitialSignals(inits)

      //TODO decide if infoHub should be own actor
      netlistActor = newNetlistActor(runId, None, Some(self))

      // send inits out; activate (lowest chanID) first
      sendOutSignals(runId, inits.toSeq.sortBy(_.channelId))

    case HandshakeActor.Signal(runId, ds, testEvent) =>
      info(s"Got signal from MainNetlist: $ds")
      val successor = informationHub.newIOEvent(ds, testEvent)
      if(ds == Acknowledge(1))  //stop for other reasons?!
        stop(Done)
      else
        mirrorSignal(runId, ds, successor)

    case nf: NormalFlowReaction =>
      trace("recording normalFlowReaction")
      // reply with TestEvent
      sender() ! informationHub.handleReaction(nf)
    case DecisionRequired(possibilities) =>
      info(s"DecisionRequired: ${possibilities.keys}")
      val ccs = createFeasibleCCs(possibilities)

      if (ccs.isEmpty) throw new RuntimeException("Cannot handle this yet.")

      //TODO: instead of deciding one could copy at this point
      val decision = decide(possibilities.filterKeys(ccs.keySet contains _))

      val newCC = ccs(decision)
      informationHub.cc = newCC

      val (reaction, newState) = possibilities(decision)
      val testEventO = informationHub.handleReaction(reaction)

      //TODO: send this to copies, too
      sender() ! Decision(newState, reaction.signals, testEventO)
  }


  private var informationHub: InformationHub = _
  private var netlistActor: ActorRef = _
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
      .foreach(netlistActor ! _)  // send them out
  }

  private def createFeasibleCCs(possibilities: DecisionPossibilities): Map[ConstraintVariable, ConstraintCollection] =
    possibilities.keys
      .map{case constraint => constraint -> informationHub.cc.fork().add(List(constraint))}
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

    val followerEvent = informationHub.newIOEvent(answerSignal, testEvent)
    sendOutSignals(nlId, Option(answerSignal), Option(followerEvent))
  }

  private def stop(reason: StopReason) = {
    info(s"I'm stopping because of: $reason")

    val (cc, tb) = informationHub.state
    info(s"Current ConstraintCollection: $cc")
    trySolving(cc, tb) match {
      case Some(test) => info(s"here is a test, anyway: $test")
      case None => info("Not even found a test.")
    }

    // TODO: give feedback to somebody
    //context.stop(self)
    context.system.terminate()
  }

  private def trySolving(cc: ConstraintCollection, tb: TestBuilder): Option[Test] = {
    val solver = new ChocoSolver(cc)
    if(solver.isFeasible) {
      Option(tb.instantiate(solver.next.apply))
    } else None
  }
}
