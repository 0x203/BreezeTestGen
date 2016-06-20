package de.hpi.asg.breezetestgen.testgeneration

import de.hpi.asg.breezetestgen.testing._
import de.hpi.asg.breezetestgen.testing.coverage.Coverage

/** A single generated test and it's coverage  */
case class GeneratedTest(test: Test, coverage: Coverage)

/** A TestGenerator returns this */
sealed trait GenerationResult
case class CompleteCoverage(tests: Set[GeneratedTest]) extends GenerationResult
case class PartialCoverage(tests: Set[GeneratedTest]) extends GenerationResult
case class GenerationError(reason: String) extends GenerationResult
