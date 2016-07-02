package de.hpi.asg.breezetestgen

import java.io.File

import de.hpi.asg.breezetestgen.domain.Netlist
import de.hpi.asg.breezetestgen.testgeneration.{CompleteCoverage, GeneratedTest, GenerationError, PartialCoverage}
import de.hpi.asg.breezetestgen.testing.JsonFromTo
import de.hpi.asg.breezetestgen.util.Logging

object Main {
  val logger = Logging.getLogger(this)
  case class Config(breezeFile: File = new File("in.breeze"),
                    testFiles: Seq[File] = Seq.empty,  // currently ignored
                    logFile: File = new File("simulator.log"),
                    logLevel: Int = 2,
                    debug: Boolean = false)

  private val context = new TestGenerationContext()

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

  def main (args: Array[String]): Unit =
    parser.parse(args, Config()).flatMap {
    case config =>
      Logging.initLogger(config.logLevel, config.logFile, debugMode = config.debug)
      context.generateTestsForFile(config.breezeFile) match {
        case CompleteCoverage(netlist, tests) =>
          logger.info("Generated test completely covering the whole netlist!")
          Some(config, netlist, tests)
        case PartialCoverage(netlist, tests) =>
          logger.info("Generated some tests, but without complete coverage.")
          Some(config, netlist, tests)
        case GenerationError(reason) =>
          logger.error(s"Something went wrong: $reason")
          None
      }
    }.map{
      case (config, netlist, tests) =>
        val rendered = JsonFromTo.testSuiteToJsonString(netlist, tests, renderCompact = false)

        import java.nio.file.{Paths, Files}
        import java.nio.charset.StandardCharsets

        Files.write(
          Paths.get("testsuite.json"),
          rendered.getBytes(StandardCharsets.UTF_8)
        )
    }
}

