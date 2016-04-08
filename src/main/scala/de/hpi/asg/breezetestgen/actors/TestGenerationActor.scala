package de.hpi.asg.breezetestgen.actors

import akka.actor.{Actor, ActorRef}
import de.hpi.asg.breezetestgen.Loggable
import de.hpi.asg.breezetestgen.domain._
import de.hpi.asg.breezetestgen.domain.components.BrzComponentBehaviour._
import de.hpi.asg.breezetestgen.testgeneration.{InformationHub, VariableData, constraintsolving}
import constraintsolving.{ChocoSolver, ConstraintCollection, ConstraintVariable, Variable}
import de.hpi.asg.breezetestgen.testing.{IOEvent, TestEvent}

object TestGenerationActor {
  case object Start

  private def createActivateRequest(nlId: Netlist.Id): HandshakeActor.Signal = {
    val ds = Request(1)
    HandshakeActor.Signal(nlId, ds, IOEvent(ds))
  }
}

class TestGenerationActor(protected val netlist: Netlist) extends Actor with MainNetlistCreator with Loggable {
  import TestGenerationActor._

  def receive = {
    case Start =>
      val runId = nextRunId; nextRunId -= 1

      val inits = initialRequests(runId)

      informationHub = InformationHub.fromInitialSignals(inits)

      //TODO decide if infoHub should be own actor
      netlistActor = newNetlistActor(runId, None, Some(self))

      // send inits out; activate (lowest chanID) first
      sendOutSignals(runId, inits.toSeq.sortBy(_.channelId))

    case HandshakeActor.Signal(runId, ds, testEvent) =>
      //TODO: simulate environment
      //TODO: tell this the informationHub for new testEvent and variables
      //TODO check if this was an acknowledge on activate and sstop if so

    case nf: NormalFlowReaction =>
      //TODO: maybe stop search?!
      // reply with Option[TestEvent]
      sender() ! informationHub.handleReaction(nf)
    case (DecisionRequired(possibilities), testEvent: TestEvent) =>
      val ccs = createFeasibleCCs(possibilities)

      if (ccs.isEmpty) throw new RuntimeException("Cannot handle this yet.")

      //TODO: instead of deciding one could copy at this point
      val decision = decide(possibilities.filterKeys(ccs.keySet contains _))

      val newCC = ccs(decision)
      val (reaction, newState) = possibilities(decision)

      informationHub.cc = newCC
      val newTestEvent = informationHub.handleReaction(reaction) getOrElse testEvent

      //TODO: set new state of forking component
      //netlistActor ! SetState(id??, newState)

      // send this on behalf of component in action
      reaction.signals.foreach(ds => {
        netlistActor ! HandshakeActor.Signal(netlist.id, ds, newTestEvent)
      })
  }


  private var informationHub: InformationHub = _
  private var netlistActor: ActorRef = _
  private var nextRunId: Int = -1

  private def initialRequests(runId: Netlist.Id): Set[Signal] = {
    netlist.activePorts.map{
      case dp: DataPort =>
        val v = new Variable(dp.name, dp.bitCount, dp.isSigned)
        val d = new VariableData(v, null)
        DataRequest(dp.channelId, d)
      case sp: SyncPort => Request(sp.channelId)
    }
  }

  private def sendOutSignals(runId: Netlist.Id, signals: Traversable[Signal]) = {
    signals
      .map{ds => HandshakeActor.Signal(runId, ds, IOEvent(ds))} // create understandable signals
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
}
