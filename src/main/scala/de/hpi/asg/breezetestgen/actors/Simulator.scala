package de.hpi.asg.breezetestgen.actors

import akka.actor.{Actor, ActorRef, Props}
import de.hpi.asg.breezetestgen.Loggable
import de.hpi.asg.breezetestgen.domain._
import de.hpi.asg.breezetestgen.testing._

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.mutable

object Simulator {
  case object RunTest

  class TestFailed(val unexpectedSignal: Signal, currentTest: mutable.Graph[TestEvent, DiEdge]) extends Exception {
    val testState = scalax.collection.immutable.Graph[TestEvent, DiEdge]() ++ currentTest
  }
}

/** performs a test on a netlist
  *
  */
class Simulator(netlist: Netlist, test: Test) extends Actor with Loggable{
  import Simulator._

  val dummyNetlistId = 0

  val activePorts = netlist.activePorts
  val passivePorts = netlist.passivePorts

  val runningTest = mutable.Graph[TestEvent, DiEdge]() ++ test

  var netlistActor: ActorRef = _

  private def pendingEvents = runningTest.nodes.filter{!_.hasPredecessors}.map(_.value)

  private def triggerActiveEvents(): Unit = {
    info("triggering active events")
    def triggerOne(): Boolean = pendingEvents.collectFirst{
      case m: MergeEvent => runningTest -= m
      case event: IOEvent[_] if activePorts contains event.port =>
        trigger(event)
        runningTest -= event
    }.isDefined

    // trigger until none is modified further
    while (triggerOne()) {info("active one triggered")}
  }

  private def trigger(event: IOEvent[_]) = {
    val dSignal = event match {
      case IOSyncEvent(port) => port.createSignal()
      case IODataEvent(port, value) => port.createSignal(value)
    }
    val hsSignal = HandshakeActor.Signal(dummyNetlistId, dSignal, event)
    info(s"Sending $hsSignal")
    netlistActor ! hsSignal
  }

  private def handleSignal(signal: Signal) = {
    val portOption = passivePorts.find(_.channelId == signal.channelId)
    val teOption = portOption.flatMap{port =>
      pendingEvents.find{case ioEvent: IOEvent[_] => ioEvent.port == port}
    }
    teOption match {
      case Some(node) =>
        runningTest -= node
      case None =>
        throw new TestFailed(signal, runningTest)
    }
    somethingHappened()
  }

  private def somethingHappened() = {
    triggerActiveEvents()
    if (runningTest.isEmpty) {
      info("Test cleared, everything is done!")
      context.stop(self)
    }
  }

  def receive = {
    case RunTest =>
      netlistActor = context.actorOf(netlistActorProps, "MainNetlist")
      somethingHappened()
    case HandshakeActor.Signal(`dummyNetlistId`, domainSignal, _) => handleSignal(domainSignal)
  }

  /** creates [[Props]] of a netlist actor for the netlist to be simulated */
  private def netlistActorProps: Props = {
    val portConnections = netlist.ports.keys.map{id => id -> id}.toMap[Port.Id, Channel.Id]
    // TODO create real infoHub when its implemented
    val infoHub = context.system.deadLetters

    Props(classOf[NetlistActor], netlist, dummyNetlistId, portConnections, infoHub)
  }
}
