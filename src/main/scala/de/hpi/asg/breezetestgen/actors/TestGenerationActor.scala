package de.hpi.asg.breezetestgen.actors

import akka.actor.{Actor, ActorRef, Terminated}
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

class TestGenerationActor(private val testGenerator: TestGenerator) extends Actor with MainNetlistCreator with Loggable {
  import TestGenerationActor._

  private var inquirer: ActorRef = _
  private val mainNetlistActors = mutable.Map.empty[Int, ActorRef]

  def receive = {
    case Start =>
      inquirer = sender()
      performActions(sender(), testGenerator.start())

    case HandshakeActor.Signal(runId, Nil, ds, testEvent) =>
      performActions(sender(), testGenerator.onPortSignal(runId, ds, testEvent))

    case HandshakeActor.NormalFlowReaction(runId, componentId, nf: NormalFlowReaction) =>
      performActions(sender(), testGenerator.onNormalFlow(runId, componentId, nf))

    case HandshakeActor.DecisionRequired(runId, componentId, DecisionRequired(possibilities)) =>
      performActions(sender(), testGenerator.onDecisionRequired(runId, componentId, possibilities))

    case MyState(runId, _, state: Netlist.State) =>
      performActions(sender(), testGenerator.onWholeState(runId, state))

    case Terminated(mainNetlistActor) =>
      mainNetlistActors.find(_._2 == mainNetlistActor) match {
        case Some((runId, _)) => performActions(sender(), testGenerator.onUnexpectedTermination(runId))
        case None => info("A netlist actor died, but I didn't knew him.")
      }
  }


  private def performActions(sender: ActorRef, actions: List[TestGenerationAction]) =
    for(action <- actions)
      performAction(sender, action)

  private def performAction(sender: ActorRef, action: TestGenerationAction) =
    action match {
      case CreateMainNetlist(runId, stateO) =>
        info(s"$runId: Creating new MainNetlist")
        val freshNetlistActor = newNetlistActor(runId, stateO, Some(self))
        mainNetlistActors += runId -> freshNetlistActor
        context.watch(freshNetlistActor)

      case StopMainNetlist(runId) =>
        info(s"$runId: Stopping main NetlistActor")
        for (actor <- mainNetlistActors.remove(runId)) {
          context.unwatch(actor)
          context.stop(actor)
        }

      case SendToMainNetlist(runId, signals) =>
        info(s"$runId: Sending signals to main netlist")
        val mainNetlistActor = mainNetlistActors(runId)
        for(signal <- signals)
          mainNetlistActor ! signal

      case RequestWholeState(runId) =>
        info(s"$runId: Requesting the main netlist's state")
        val mainNetlistActor = mainNetlistActors(runId)
        mainNetlistActor ! GetState

      case ReturnTestEvent(testEvent) =>
        trace("sending new test Event to component")
        sender ! testEvent

      case SendDecision(runId, decision) =>
        info(s"$runId: Sending decision to netlist: $decision")
        val mainNetlistActor = mainNetlistActors(runId)
        mainNetlistActor ! decision

      case FinishedGeneration(result) =>
        info("finished test generation, will stop now.")
        inquirer ! result
        context.stop(self)
    }

  // this is needed for implementing the mainNetlistCreator interface
  protected def netlist: Netlist = testGenerator.netlist
}
