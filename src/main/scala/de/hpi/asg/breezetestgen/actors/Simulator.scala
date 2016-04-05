package de.hpi.asg.breezetestgen.actors

import akka.actor.{Actor, ActorRef, Props}
import de.hpi.asg.breezetestgen.Loggable
import de.hpi.asg.breezetestgen.domain._
import de.hpi.asg.breezetestgen.testing._

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.mutable

object Simulator {
  case object RunTest

  sealed trait TestResult
  case object TestSucceeded extends TestResult
  case class TestFailed(unexpectedSignal: Signal, currentTest: mutable.Graph[TestEvent, DiEdge]) extends TestResult
}

/** performs a test on a netlist
  *
  */
class Simulator(netlist: Netlist, test: Test) extends Actor with Loggable{
  import Simulator._

  val dummyNetlistId = 0

  var testStarter: ActorRef = _

  val activeChannels = netlist.activePorts.map{_.channelId}
  val passiveChannels = netlist.passivePorts.map{_.channelId}

  val runningTest = mutable.Graph[TestEvent, DiEdge]() ++ test

  var netlistActor: ActorRef = _

  // hacky way to create channelMaps with all ports of MainNetlist pointing to this simulator-actor
  private val netlistChannelMap = HandshakeActor.SetChannels(
    netlist.ports.map{case (_, port) => port.channelId -> SyncChannel(port.channelId, self, self)}
  )

  private def pendingEvents = runningTest.nodes.filter{!_.hasPredecessors}.map(_.value)

  /** if the simulator is responsible for sending this */
  private def myOwn(s: Signal): Boolean = s match {
    case a: SignalFromActive => activeChannels contains a.channelId
    case p: SignalFromPassive => passiveChannels contains p.channelId
  }

  private def triggerOwnEvents(): Unit = {
    info("triggering active events")
    def triggerOne(): Boolean = pendingEvents.collectFirst{
      case m: MergeEvent => runningTest -= m
      case event @ IOEvent(s) if myOwn(s) =>
        trigger(event)
        runningTest -= event
    }.isDefined

    // trigger until none is modified further
    while (triggerOne()) {trace("one own triggered")}
  }

  private def trigger(event: IOEvent) = {
    val hsSignal = HandshakeActor.Signal(dummyNetlistId, event.signal, event)
    info(s"Sending $hsSignal")
    netlistActor ! hsSignal
  }

  private def handleSignal(signal: Signal) = {
    pendingEvents.find{case ioEvent: IOEvent => ioEvent.signal == signal} match {
      case Some(event: IOEvent) => runningTest -= event
      case None => testFailed(signal)
    }
    somethingHappened()
  }

  private def somethingHappened() = {
    triggerOwnEvents()
    if (runningTest.isEmpty)
      testSucceeded()
  }

  private def testSucceeded() = {
    info("Test cleared, everything is done!")
    testDone(TestSucceeded)
  }
  private def testFailed(signal: Signal) = {
    info(s"Test failed at signal: $signal")
    testDone(TestFailed(signal, runningTest))
  }
  private def testDone(result: TestResult) = {
    context.stop(netlistActor)
    testStarter ! result
  }

  def receive = {
    case RunTest =>
      testStarter = sender()
      netlistActor = context.actorOf(netlistActorProps, "MainNetlist")
      netlistActor ! netlistChannelMap
      somethingHappened()
    case HandshakeActor.Signal(`dummyNetlistId`, domainSignal, _) => handleSignal(domainSignal)
  }

  /** creates [[Props]] of a netlist actor for the netlist to be simulated */
  private def netlistActorProps: Props = {
    val portConnections = netlist.ports.values.map{p => p.id -> p.channelId}.toMap[Port.Id, Channel.Id]
    // TODO create real infoHub when its implemented
    val infoHub = context.system.deadLetters

    Props(classOf[NetlistActor], netlist, dummyNetlistId, portConnections, infoHub)
  }
}
