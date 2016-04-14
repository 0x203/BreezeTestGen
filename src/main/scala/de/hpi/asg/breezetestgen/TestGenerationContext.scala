package de.hpi.asg.breezetestgen

import com.typesafe.config._
import de.hpi.asg.breezetestgen.domain.Netlist
import de.uni_potsdam.hpi.asg.common.io.WorkingdirGenerator


class TestGenerationContext(config: Config) extends Loggable {

  config.checkValid(ConfigFactory.defaultReference(), "breeze-test-gen")

  def this() {
    this(ConfigFactory.load())
  }

  //TODO: specify proper return values and use them

  def generateTestsForFile(breezeFile: java.io.File) = {
    logger.info(s"Execute for file: ${breezeFile.getName}")
    val mainNetlist = surroundWithTempDir{ BreezeTransformer.parse(breezeFile) }
    mainNetlist match {
      case Some(netlist) =>
        logger.info(netlist)
        generateTestsForNetlist(netlist)
      case None => logger.error("Could not parse Netlist")
    }
  }

  def generateTestsForNetlist(mainNetlist: Netlist) = {
    import akka.actor.{ActorSystem, Props}
    import scala.concurrent.Await
    import scala.concurrent.duration._

    import actors.TestGenerationActor

    val system = ActorSystem("TestGen")
    logger.info("Start testfinding...")
    val testgen = system.actorOf(Props(classOf[TestGenerationActor], mainNetlist))
    testgen ! TestGenerationActor.Start
    Await.result(system.whenTerminated, 20 seconds)
    logger.info("testfinding finished!")
  }

  private def surroundWithTempDir[R](f: => R): R =
    config.getBoolean("breeze-test-gen.use-tmp-dir") match {
      case false => f
      case true =>
        WorkingdirGenerator.getInstance.create(null, null, "BrzTestGenTmp", null)
        val r: R = f
        WorkingdirGenerator.getInstance.delete()
        r
    }
}
