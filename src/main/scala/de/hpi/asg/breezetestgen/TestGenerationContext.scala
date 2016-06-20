package de.hpi.asg.breezetestgen

import com.typesafe.config._
import de.hpi.asg.breezetestgen.domain.Netlist
import de.hpi.asg.breezetestgen.testgeneration._
import de.hpi.asg.breezetestgen.testing.JsonFromTo


class TestGenerationContext(config: Config) extends Loggable {

  config.checkValid(ConfigFactory.defaultReference(), "breeze-test-gen")

  def this() {
    this(ConfigFactory.load())
  }

  //TODO: specify proper return values and use them

  def generateTestsForFile(breezeFile: java.io.File): GenerationResult = {
    info(s"Execute for file: ${breezeFile.getName}")
    val mainNetlist = BreezeTransformer.parse(breezeFile)(config)
    mainNetlist match {
      case Some(netlist) =>
        info(netlist.toString)
        generateTestsForNetlist(netlist)
      case None =>
        error("Could not parse Netlist")
        GenerationError("Could not parse netlist")
    }
  }

  def generateTestsForNetlist(mainNetlist: Netlist): GenerationResult = {
    import akka.actor.{ActorSystem, Props, Inbox}
    import scala.concurrent.duration._

    import actors.TestGenerationActor

    val system = ActorSystem("TestGen")
    info("Start testfinding...")
    val testGenerator = system.actorOf(Props(classOf[TestGenerationActor], mainNetlist))
    val box = Inbox.create(system)

    box.send(testGenerator, TestGenerationActor.Start)
    val result = box.receive(20 seconds).asInstanceOf[GenerationResult]

    info("generation of tests finished!")
    system.terminate()
    result
  }
}
