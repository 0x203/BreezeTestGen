package de.hpi.asg.breezetestgen.actors

import akka.actor.{Actor, ActorRef, Props}
import de.hpi.asg.breezetestgen.Loggable
import de.hpi.asg.breezetestgen.domain._
import de.hpi.asg.breezetestgen.testing._

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.mutable.{Graph => MutableGraph}

object Simulator {
  case class RunTest(test: Test)

  sealed trait TestResult
  case object TestSucceeded extends TestResult
  case class TestFailed(unexpectedSignal: Signal, currentTest: MutableGraph[TestEvent, DiEdge]) extends TestResult

  private type RunningTest = MutableGraph[TestEvent, DiEdge]
  private type TestId = Netlist.Id
}

/** performs a test on a netlist
  *
  */
class Simulator(netlist: Netlist) extends Actor with Loggable{
  import Simulator._

  def receive = {
    case RunTest(test) => newTest(test, sender())
    case HandshakeActor.Signal(testId, domainSignal, _) => handleSignal(domainSignal)(testId)
  }

  private val activeChannels = netlist.activePorts.map{_.channelId}
  private val passiveChannels = netlist.passivePorts.map{_.channelId}

  /** if the simulator is responsible for sending this */
  private def myOwn(s: Signal): Boolean = s match {
    case a: SignalFromActive => activeChannels contains a.channelId
    case p: SignalFromPassive => passiveChannels contains p.channelId
  }

  private def triggerOwnEvents()(implicit testId: TestId): Unit = {
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

  private def trigger(event: IOEvent)(implicit testId: TestId) = {
    val hsSignal = HandshakeActor.Signal(testId, event.signal, event)
    info(s"Sending $hsSignal")
    netlistActor ! hsSignal
  }

  private def handleSignal(signal: Signal)(implicit testId: TestId) = {
    pendingEvents.find{case ioEvent: IOEvent => ioEvent.signal == signal} match {
      case Some(event: IOEvent) => runningTest -= event
      case None => testFailed(signal)
    }
    somethingHappened()
  }

  private def somethingHappened()(implicit testId: TestId) = {
    triggerOwnEvents()
    if (runningTest.isEmpty)
      testSucceeded()
  }

  private def testSucceeded()(implicit testId: TestId) = {
    info("Test cleared, everything is done!")
    testDone(TestSucceeded)
  }
  private def testFailed(signal: Signal)(implicit testId: TestId) = {
    info(s"Test failed at signal: $signal")
    testDone(TestFailed(signal, runningTest))
  }
  private def testDone(result: TestResult)(implicit testId: TestId) = {
    context.stop(netlistActor)
    invokers(testId) ! result
  }


  private var nextTestId = -1
  private val runningTests: collection.mutable.Map[TestId, RunningTest] = collection.mutable.Map.empty
  private val invokers: collection.mutable.Map[TestId, ActorRef] = collection.mutable.Map.empty
  private val netlistActors: collection.mutable.Map[TestId, ActorRef] = collection.mutable.Map.empty

  private implicit def runningTest(implicit testId: TestId): RunningTest = runningTests(testId)
  private def pendingEvents(implicit runningTest: RunningTest) =
    runningTest.nodes.filter{!_.hasPredecessors}.map(_.value)
  private def netlistActor(implicit testId: TestId) = netlistActors(testId)

  private def newTest(test: Test, invoker: ActorRef) = {
    val testId = nextTestId; nextTestId -= 1

    runningTests += testId -> (MutableGraph[TestEvent, DiEdge]() ++ test)
    invokers += testId -> sender()
    netlistActors += testId -> newNetlistActor(testId)
    somethingHappened()(testId)
  }

  // hacky way to create channelMaps with all ports of MainNetlist pointing to this simulator-actor
  private val netlistChannelMap = HandshakeActor.SetChannels(
    netlist.ports.map{case (_, port) => port.channelId -> SyncChannel(port.channelId, self, self)}
  )

  /** creates a new netlist actor for the netlist to be simulated */
  private def newNetlistActor(id: Netlist.Id): ActorRef = {
    val portConnections = netlist.ports.values.map{p => p.id -> p.channelId}.toMap[Port.Id, Channel.Id]
    // TODO create real infoHub when its implemented
    val infoHub = context.system.deadLetters
    val props = Props(classOf[NetlistActor], netlist, id, portConnections, infoHub)

    val newActor = context.actorOf(props, s"Test${id}-MainNetlist")
    newActor ! netlistChannelMap
    newActor
  }
}
