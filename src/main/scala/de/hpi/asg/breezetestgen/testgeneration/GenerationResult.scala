package de.hpi.asg.breezetestgen.testgeneration

import de.hpi.asg.breezetestgen.domain.Netlist
import de.hpi.asg.breezetestgen.testing._
import de.hpi.asg.breezetestgen.testing.coverage.Coverage

/** A single generated test and it's coverage  */
case class GeneratedTest(test: Test, coverage: Coverage)

/** A TestGenerator returns this */
sealed trait GenerationResult
case class CompleteCoverage(netlist: Netlist, tests: Set[GeneratedTest]) extends GenerationResult
case class PartialCoverage(netlist: Netlist, tests: Set[GeneratedTest]) extends GenerationResult
case class GenerationError(reason: String) extends GenerationResult
