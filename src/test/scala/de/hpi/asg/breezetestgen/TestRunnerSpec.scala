package de.hpi.asg.breezetestgen

import de.hpi.asg.breezetestgen.baseclasses.UnitTest
import de.hpi.asg.breezetestgen.domain._
import de.hpi.asg.breezetestgen.fixtures.GCDTest
import de.hpi.asg.breezetestgen.testing.TestRunner
import de.hpi.asg.breezetestgen.testing.TestRunner._

class TestRunnerSpec extends UnitTest {
  val (ain, bin, o) = (16, 12, 4)
  val test = GCDTest(ain, bin, o)

  val activateChan = 1
  val ainChan = 2
  val binChan = 3
  val oChan = 4

  def shouldEmit(s: Signal): Boolean = s match {
    case _: SignalFromActive => s.channelId == activateChan
    case _: SignalFromPassive => Set(2,3,4) contains s.channelId //ain, bin and o channel Ids
  }

  "The TestRunner" should "behave correctly for a workingTest" in {
    val testRunner = new TestRunner(test, shouldEmit _)

    assertResult(
      EmitSignals(Set(Request(activateChan)))
    ) {
      testRunner.firstAction()
    }

    assertResult(
      EmitSignals(Set(DataAcknowledge(ainChan, Constant(ain))))
    ) {
      testRunner.reactToSignal(Request(ainChan))
    }

    assertResult(
      EmitSignals(Set(DataAcknowledge(binChan, Constant(bin))))
    ) {
      testRunner.reactToSignal(Request(binChan))
    }

    assertResult(
      EmitSignals(Set(Acknowledge(oChan)))
    ) {
      testRunner.reactToSignal(DataRequest(oChan, Constant(o)))
    }

    assertResult(
      TestSucceeded
    ) {
      testRunner.reactToSignal(Acknowledge(activateChan))
    }
  }

  it should "react with TestFailed if an unexpected signal occurs" in {
    val testRunner = new TestRunner(test, shouldEmit _)

    assertResult(
      EmitSignals(Set(Request(activateChan)))
    ) {
      testRunner.firstAction()
    }

    assertResult(
      EmitSignals(Set(DataAcknowledge(ainChan, Constant(ain))))
    ) {
      testRunner.reactToSignal(Request(ainChan))
    }

    //until here everything was fine, but then, way too early, activate gets acknowledged

    val wrongSignal = Acknowledge(activateChan)
    val reaction= testRunner.reactToSignal(wrongSignal) //BAM! early acknowledge

    assert(reaction.isInstanceOf[TestFailed])
    assert(reaction.asInstanceOf[TestFailed].unexpectedSignal == wrongSignal)
  }
}
