package de.hpi.asg.breezetestgen.actors

import akka.actor.{Actor, Props}
import de.hpi.asg.breezetestgen.Loggable
import de.hpi.asg.breezetestgen.domain._
import de.hpi.asg.breezetestgen.testing._

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.mutable

object Simulator {
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
  val netlistActor = context.actorOf(netlistActorProps, "MainNetlist")

  // ports which are passive as seen from the netlist itself are active for the environment and the other way round
  val activePorts = netlist.passivePorts
  val passivePorts = netlist.activePorts

  val runningTest = mutable.Graph[TestEvent, DiEdge]() ++ test

  private def pendingEvents = runningTest.nodes.filter{!_.hasPredecessors}

  private def triggerActiveEvents(): Unit = {
    def triggerOne(): Boolean = pendingEvents.collectFirst{
      case m: MergeEvent => runningTest -= m
      case event: IOEvent[_] if activePorts contains event.port =>
        trigger(event)
        runningTest -= event
    }.isDefined

    // trigger until none is modified further
    while (triggerOne()) {}
  }

  private def trigger(event: IOEvent[_]) = {
    val dSignal = event match {
      case IOSyncEvent(port) => port.createSignalFromOutside()
      case IODataEvent(port, value) => port.createSignalFromOutside(value)
    }
    val hsSignal = HandshakeActor.Signal(dummyNetlistId, dSignal, event)
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

  somethingHappened()

  def receive = {
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
