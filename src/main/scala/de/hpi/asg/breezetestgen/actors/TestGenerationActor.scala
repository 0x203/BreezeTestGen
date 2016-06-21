package de.hpi.asg.breezetestgen.actors

import akka.actor.{Actor, ActorRef}
import de.hpi.asg.breezetestgen.Loggable
import de.hpi.asg.breezetestgen.domain._
import components.BrzComponentBehaviour.{DecisionRequired, NormalFlowReaction}
import de.hpi.asg.breezetestgen.testgeneration.TestGenerator
import de.hpi.asg.breezetestgen.actors.HandshakeActor.{GetState, MyState}
import de.hpi.asg.breezetestgen.testgeneration.TestGenerator._

import scala.collection.mutable

object TestGenerationActor {
  case object Start
}

class TestGenerationActor(protected val netlist: Netlist) extends Actor with MainNetlistCreator with Loggable {
  import TestGenerationActor._

  private val testGenerator = new TestGenerator(netlist)
  private var inquirer: ActorRef = _
  private val mainNetlistActors = mutable.Map.empty[Netlist.Id, ActorRef]


  def receive = {
    case Start =>
      inquirer = sender()
      performActions(testGenerator.start())

    case HandshakeActor.Signal(runId :: _, ds, testEvent) =>
      performActions(testGenerator.onPortSignal(runId, ds, testEvent))

    case HandshakeActor.NormalFlowReaction(runId :: _, nf: NormalFlowReaction) =>
      // reply with TestEvent
      sender() ! testGenerator.onNormalFlow(runId, nf)

    case HandshakeActor.DecisionRequired(runId:: idChain, componentId, DecisionRequired(possibilities)) =>
      performActions(testGenerator.onDecisionRequired(runId, idChain, componentId, possibilities))

    case MyState(runId :: _, _, state: Netlist.State) =>
      performActions(testGenerator.onWholeState(runId, state))
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
}
