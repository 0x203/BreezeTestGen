package de.hpi.asg.breezetestgen

import java.io.File

import akka.actor.{ActorSystem, Props}
import de.hpi.asg.breezetestgen.actors.Simulator
import de.uni_potsdam.hpi.asg.common.io.WorkingdirGenerator

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.immutable.Graph

object Main {
  val logger = Logging.getLogger(this)
  case class Config(breezeFile: File = new File("in.breeze"),
                    testFiles: Seq[File] = Seq.empty,  // currently ignored
                    logFile: File = new File("simulator.log"),
                    logLevel: Int = 2,
                    debug: Boolean = false)

  val parser = new scopt.OptionParser[Config]("BrzTestGen") {
    head("BrzTestGen", "0.1")
    opt[Int]("logLevel") action { (x, c) =>
      c.copy(logLevel = x) } validate { x =>
      if (x >= 0 & x < 5) success else failure("Value <logLevel> must be >=0 and <5")
    } text "logLevel"
    opt[Unit]("debug") hidden() action { (_, c) =>
      c.copy(debug = true) }
    arg[File]("<breezeFile>") action { (x, c) =>
      c.copy(breezeFile = x) } text "Breeze Netlist to simulate"
    arg[File]("<testFile>...") unbounded() optional() action { (x, c) =>
      c.copy(testFiles = c.testFiles :+ x) } text "Tests to run"
    opt[File]("logFile") optional() valueName "<file>" action { (x, c) =>
      c.copy(logFile = x) } text "file to log into"

    help("help") text "Generates tests out of Breeze netlists"
  }

  def main (args: Array[String]) = parser.parse(args, Config()) match {
    case Some(config) =>
      Logging.initLogger(config.logLevel, config.logFile, debugMode = config.debug)
      WorkingdirGenerator.getInstance.create(null, null, "BrzTestGenTmp", null)
      execute(config.breezeFile)
      WorkingdirGenerator.getInstance.delete()
    case None =>
      sys.exit(-1)
  }

  private def execute(breezeFile: File) = {
    logger.info(s"Execute for file: ${breezeFile.getName}")
    val mainNetlist = BreezeTransformer.parse(breezeFile)
    //TODO: implement me
    mainNetlist match {
      case Some(netlist) =>
        logger.info(netlist)
        val test = sampleGCDTest(netlist)
        simulate(netlist, test)
      case None => logger.error("Could not parse Netlist")
    }
  }

  private def sampleGCDTest(netlist: domain.Netlist): testing.Test = {
    import de.hpi.asg.breezetestgen.domain._
    import de.hpi.asg.breezetestgen.testing._
    def findPort(name: String): Port = netlist.ports.values.find(_.name == name).get
    val activate = IOSyncEvent(findPort("activate").asInstanceOf[SyncPort])
    val ain = IODataEvent(findPort("ain").asInstanceOf[DataPort], Constant(12))
    val bin = IODataEvent(findPort("bin").asInstanceOf[DataPort], Constant(8))
    val merge = new MergeEvent
    val o = IODataEvent(findPort("o").asInstanceOf[DataPort], Constant(4))
    Graph[TestEvent, DiEdge](
      activate ~> ain,
      activate ~> bin,
      ain ~> merge,
      bin ~> merge,
      merge ~> o
    )
  }

  private def simulate(netlist: domain.Netlist, test: testing.Test) = {
    val system = ActorSystem("Simulator")
    logger.info("Start simulation...")
    val simulator = system.actorOf(Props(classOf[Simulator], netlist, test))
    simulator ! Simulator.RunTest
    Await.result(system.whenTerminated, Duration.Inf)
    logger.info("Simulation finished!")
  }
}

