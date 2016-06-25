package de.hpi.asg.breezetestgen

import java.util.concurrent.TimeoutException

import com.typesafe.config._
import de.hpi.asg.breezetestgen.domain.Netlist
import de.hpi.asg.breezetestgen.testgeneration._

import scala.concurrent.duration.Duration


class TestGenerationContext(config: Config) extends Loggable {

  config.checkValid(ConfigFactory.defaultReference(), "breeze-test-gen")
  private val timeout = Duration.fromNanos(config.getDuration("breeze-test-gen.test-generation-timeout").toNanos)

  def this() {
    this(ConfigFactory.load())
  }

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
    import actors.TestGenerationActor

    val testGenerator = createTestGenerator(mainNetlist)
    val system = ActorSystem("TestGen")
    info("Start testfinding...")
    val testGenerationActor = system.actorOf(Props(classOf[TestGenerationActor], testGenerator))
    val box = Inbox.create(system)

    box.send(testGenerationActor, TestGenerationActor.Start)
    try {
      val result = box.receive(timeout)

      info("generation of tests finished!")
      result.asInstanceOf[GenerationResult]

    } catch {
      case _:TimeoutException =>
        warn("generation of tests timed out")
        GenerationError("Timeout for generation!")
    } finally {
      system.terminate()
    }
  }

  private def createTestGenerator(mainNetlist: Netlist): TestGenerator = {
    val maxLoopExecs = config.getInt("breeze-test-gen.max-loop-executions")
    new TestGenerator(mainNetlist, maxLoopExecs)
  }
}
