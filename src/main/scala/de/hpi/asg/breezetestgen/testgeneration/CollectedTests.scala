package de.hpi.asg.breezetestgen.testgeneration

import de.hpi.asg.breezetestgen.testing.coverage.Coverage
import de.hpi.asg.breezetestgen.util.Loggable


class CollectedTests(private val emptyCoverage: Coverage) extends Loggable {
  val tests = scala.collection.mutable.Set.empty[GeneratedTest]

  /** registers a newly found test*/
  def foundNewTest(test: GeneratedTest): Unit = {
    info(s"Found a test with ${test.coverage.percentageCovered}% coverage.")
    if (test.coverage.isComplete) {
      info(s"clearing all ${tests.size} tests found so far in favour of the complete one")
      tests.clear()
      tests.add(test)
    } else {
      addAndCheckRedundancy(test)
    }
  }

  /**  coverage of all tests so far combined */
  def combinedCoverage: Coverage =
    if(tests.isEmpty)
      emptyCoverage
    else
      tests.map(_.coverage).reduce(_ merge _)

  /** returns the minimal set of tests for maximal coverage so far */
  def testCollection: Set[GeneratedTest] = tests.toSet

  /** add test if its increases coverage, remove other tests that become superfluous because of that*/
  private def addAndCheckRedundancy(test: GeneratedTest): Unit = {
    // just keep tests that cover more than the new one
    // reverse logic assures that incomparable coverages are retained
    tests.retain{other => !(other.coverage < test.coverage)}

    // add new test if it increases the combined coverage
    // or at least is not "sub-covering" another test
    if(combinedCoverage.merge(test.coverage) > combinedCoverage | !tests.exists(_.coverage >= test.coverage)) {
      info("adding a new test to the collection")
      tests += test
    }
  }
}
