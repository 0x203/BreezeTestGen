package de.hpi.asg.breezetestgen.actors

import akka.actor.{Actor, ActorRef, Props}
import de.hpi.asg.breezetestgen.Loggable
import de.hpi.asg.breezetestgen.domain._
import de.hpi.asg.breezetestgen.testing.TestRunner._
import de.hpi.asg.breezetestgen.testing._

object Simulator {
  case class RunTest(test: Test)

  private case class RunningTest(testRunner: TestRunner, invoker: ActorRef, netlistActor: ActorRef)
  private type TestId = Int
}

/** performs a test on a netlist
  *
  */
class Simulator(protected val netlist: Netlist) extends Actor with MainNetlistCreator with Loggable{
  import Simulator._

  def receive = {
    case RunTest(test) => newTest(test, sender())
    case HandshakeActor.Signal(testId, Nil, domainSignal, _) => handleSignal(testId, domainSignal)
  }

  private def newTest(test: Test, invoker: ActorRef) = {
    val testId = nextTestId; nextTestId -= 1

    val runningTest = RunningTest(new TestRunner(test, myOwn _), sender(), newNetlistActor(testId))
    runningTests += testId -> runningTest

    handleReaction(testId, runningTest, runningTest.testRunner.firstAction())
  }

  private def handleSignal(testId: TestId, signal: Signal) = {
    val runningTest = runningTests(testId)
    val reaction = runningTest.testRunner.reactToSignal(signal)
    handleReaction(testId, runningTest, reaction)
  }

  private def handleReaction(testId: TestId, runningTest: RunningTest, reaction: TestReaction) =
    reaction match {
      case result: TestResult =>
        info(s"Got TestResult for test $testId: $result")
        runningTest.invoker ! result
        context.stop(runningTest.netlistActor)
        runningTests -= testId
      case EmitSignals(signals) =>
        signals.map(packSignal(testId, _)).foreach(runningTest.netlistActor ! _)
  }

  private def packSignal(testId: TestId, signal: Signal): HandshakeActor.Signal =
    HandshakeActor.Signal(testId, Nil, signal, IOEvent(signal))

  private var nextTestId = -1
  private val runningTests = collection.mutable.Map.empty[TestId, RunningTest]

  private val activeChannels = netlist.activePorts.map{_.channelId}
  private val passiveChannels = netlist.passivePorts.map{_.channelId}

  /** if the simulator is responsible for sending this */
  private def myOwn(s: Signal): Boolean = s match {
    case a: SignalFromActive => activeChannels contains a.channelId
    case p: SignalFromPassive => passiveChannels contains p.channelId
  }
}
