package de.hpi.asg.breezetestgen

import akka.actor.Props
import de.hpi.asg.breezetestgen.actors.Simulator
import Simulator.RunTest
import de.hpi.asg.breezetestgen.testing.TestRunner.{TestFailed, TestResult, TestSucceeded}
import fixtures.gcdNetlist

/** tests a complete simulation run: providing it to a Simulator(-Actor) and run it */
class SimulationSpec extends baseclasses.AkkaIntegrationSpec("SimulationSpec") {
  val netlist = gcdNetlist()
  val workingTest = fixtures.GCDTest(12, 8, 4)
  val workingTest2 = fixtures.GCDTest(4, 8, 4)
  val brokenTest = fixtures.GCDTest(12, 8, 3)

  def newSimulator() = system.actorOf(Props(classOf[Simulator], netlist))

  "The simulator" should "answer with a result" in {
    val simulator = newSimulator()
    simulator ! RunTest(workingTest)
    expectMsgType[TestResult]
  }

  it should "answer with TestSucceeded for working tests" in {
    val simulator = newSimulator()
    simulator ! RunTest(workingTest)
    expectMsg(TestSucceeded)
  }

  it should "answer with TestFailed for broken tests" in {
    val simulator = newSimulator()
    simulator ! RunTest(brokenTest)
    expectMsgClass(classOf[TestFailed])
  }

  it should "be able to run multiple tests" in {
    val simulator = newSimulator()
    simulator ! RunTest(workingTest)
    expectMsg(TestSucceeded)
    simulator ! RunTest(workingTest2)
    expectMsg(TestSucceeded)
    simulator ! RunTest(brokenTest)
    expectMsgClass(classOf[TestFailed])
  }
}
