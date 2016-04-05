package de.hpi.asg.breezetestgen

import akka.actor.Props
import de.hpi.asg.breezetestgen.actors.Simulator
import fixtures.gcdNetlist

/** tests a complete simulation run: providing it to a Simulator(-Actor) and run it */
class SimulationSpec extends baseclasses.AkkaIntegrationSpec("SimulationSpec") {
  val netlist = gcdNetlist()
  val workingTest = fixtures.GCDTest(12, 8, 4)
  val brokenTest = fixtures.GCDTest(12, 8, 3)

  "The simulator" should "answer with a result" in {
    val simulator = system.actorOf(Props(classOf[Simulator], netlist, workingTest))
    simulator ! Simulator.RunTest
    expectMsgType[Simulator.TestResult]
  }

  it should "answer with TestSucceeded for working tests" in {
    val simulator = system.actorOf(Props(classOf[Simulator], netlist, workingTest))
    simulator ! Simulator.RunTest
    expectMsg(Simulator.TestSucceeded)
  }

  it should "answer with TestFailed for broken tests" in {
    val simulator = system.actorOf(Props(classOf[Simulator], netlist, brokenTest))
    simulator ! Simulator.RunTest
    expectMsgClass(classOf[Simulator.TestFailed])
  }
}
