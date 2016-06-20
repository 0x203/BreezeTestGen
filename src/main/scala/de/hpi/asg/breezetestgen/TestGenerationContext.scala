package de.hpi.asg.breezetestgen

import com.typesafe.config._
import de.hpi.asg.breezetestgen.domain.Netlist


class TestGenerationContext(config: Config) extends Loggable {

  config.checkValid(ConfigFactory.defaultReference(), "breeze-test-gen")

  def this() {
    this(ConfigFactory.load())
  }

  //TODO: specify proper return values and use them

  def generateTestsForFile(breezeFile: java.io.File) = {
    info(s"Execute for file: ${breezeFile.getName}")
    val mainNetlist = BreezeTransformer.parse(breezeFile)(config)
    mainNetlist match {
      case Some(netlist) =>
        info(netlist.toString)
        generateTestsForNetlist(netlist)
      case None => error("Could not parse Netlist")
    }
  }

  def generateTestsForNetlist(mainNetlist: Netlist) = {
    import akka.actor.{ActorSystem, Props, Inbox}
    import scala.concurrent.duration._

    import actors.TestGenerationActor

    val system = ActorSystem("TestGen")
    info("Start testfinding...")
    val testGenerator = system.actorOf(Props(classOf[TestGenerationActor], mainNetlist))
    val box = Inbox.create(system)

    box.send(testGenerator, TestGenerationActor.Start)
    box.receive(20 seconds) match {
      case tests: Set[TestGenerationActor.GeneratedTest] => info(s"Generated some tests: $tests")
      case x => error(s"Didn't expect this: $x")
    }

    system.terminate()
    info("testfinding finished!")
  }
}
