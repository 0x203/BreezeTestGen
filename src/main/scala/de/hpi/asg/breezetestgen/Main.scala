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
    val activateId = findPort("activate").channelId
    val ainId = findPort("ain").channelId
    val binId = findPort("bin").channelId
    val oId = findPort("o").channelId

    val activateReq = IOEvent(Request(activateId))
    val activateAck = IOEvent(Acknowledge(activateId))

    val ainReq = IOEvent(Request(ainId))
    val ainAck = IOEvent(DataAcknowledge(ainId, Constant(12)))

    val binReq = IOEvent(Request(binId))
    val binAck = IOEvent(DataAcknowledge(binId, Constant(8)))

    val oReq = IOEvent(DataRequest(oId, Constant(4)))
    val oAck = IOEvent(Acknowledge(oId))

    val merge = new MergeEvent

    Graph[TestEvent, DiEdge](
      activateReq ~> ainReq,
      activateReq ~> binReq,
      ainReq ~> ainAck,
      binReq ~> binAck,
      ainAck ~> merge,
      binAck ~> merge,
      merge ~> oReq,
      oReq ~> oAck,
      oAck ~> activateAck
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

