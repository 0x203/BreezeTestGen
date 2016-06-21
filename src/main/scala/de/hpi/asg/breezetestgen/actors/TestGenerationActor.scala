package de.hpi.asg.breezetestgen.actors

import akka.actor.{Actor, ActorRef}
import de.hpi.asg.breezetestgen.Loggable
import de.hpi.asg.breezetestgen.domain._
import de.hpi.asg.breezetestgen.testgeneration._
import de.hpi.asg.breezetestgen.testgeneration.constraintsolving._
import de.hpi.asg.breezetestgen.actors.ComponentActor.Decision
import de.hpi.asg.breezetestgen.actors.HandshakeActor.{GetState, MyState}
import components.BrzComponentBehaviour.{DecisionPossibilities, DecisionRequired, NormalFlowReaction}
import de.hpi.asg.breezetestgen.testgeneration.TestGenerator._
import de.hpi.asg.breezetestgen.testing.coverage.ChannelActivationCoverage

import scala.collection.mutable

object TestGenerationActor {
  case object Start

  private[this] var curRunId = -1
  private def nextRunId(): Netlist.Id = {
    val r = curRunId
    curRunId -= 1
    r
  }

}

class TestGenerationActor(protected val netlist: Netlist) extends Actor with MainNetlistCreator with Loggable {
  import TestGenerationActor._

  val testGenerator = new TestGenerator(netlist)
  var inquirer: ActorRef = _
  val mainNetlistActors = mutable.Map.empty[Netlist.Id, ActorRef]

  val collectedTests = new CollectedTests(ChannelActivationCoverage.forNetlist(netlist))


  def receive = {
    case Start =>
      inquirer = sender()
      val runId = nextRunId()

      performActions(testGenerator.start(runId))

    case HandshakeActor.Signal(runId :: _, ds, testEvent) =>
      info(s"Got signal from MainNetlist: $ds")
      performActions(testGenerator.onPortSignal(runId, ds, testEvent))

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
      running.update(runId, (InformationHub.fromState(netlist, resumable.infoHubState), netlistActor))

      info("send decision to inquirer")
      netlistActor ! resumable.decision
  }


  private def performActions(actions: List[TestGenerationAction]) =
    for(action <- actions)
      performAction(action)

  private def performAction(action: TestGenerationAction) =
    action match {
      case CreateMainNetlist(runId, stateO) =>
        info(s"$runId: Creating new MainNetlist")
        mainNetlistActors += runId -> newNetlistActor(runId, stateO, Some(self))

      case StopMainNetlist(runId) =>
        info(s"$runId: Stopping main NetlistActor")
        for (actor <- mainNetlistActors.remove(runId))
          context.stop(actor)

      case SendToMainNetlist(runId, signals) =>
        info(s"$runId: Sending signals to main netlist")
        val mainNetlistActor = mainNetlistActors(runId)
        for(signal <- signals)
          mainNetlistActor ! signal

      case RequestWholeState(runId) =>
        info(s"$runId: Requesting the main netlist's state")
        val mainNetlistActor = mainNetlistActors(runId)
        mainNetlistActor ! GetState

      case SendDecision(runId, decision) =>
        info(s"$runId: Sending decision to netlist")
        val mainNetlistActor = mainNetlistActors(runId)
        mainNetlistActor ! decision

      case FinishedGeneration(result) =>
        info("finished test generation, will stop now.")
        inquirer ! result
        context.stop(self)
    }



  private var resumable2BContinued: ResumableRun = _
  private var waitingForState: Set[Netlist.Id] = _
  private val backlog = mutable.Map.empty[Netlist.Id, ResumableRun]
  private val running = mutable.Map.empty[Netlist.Id, (InformationHub, ActorRef)]

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

}
