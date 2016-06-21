package de.hpi.asg.breezetestgen.testgeneration

import de.hpi.asg.breezetestgen.Loggable


class CollectedTests extends Loggable {
  val tests = scala.collection.mutable.Set.empty[GeneratedTest]

  /** registers a newly found test*/
  def foundNewTest(test: GeneratedTest): Unit = {
    info(s"Found a test with ${test.coverage.percentageCovered}% coverage.")
    if (test.coverage.isComplete) {
      tests.clear()
      tests.add(test)
    } else {
      //TODO: check if this test is superfluous or makes another test superfluous
      tests += test
    }
  }

  /** returns, if coverage hits 100% */
  def coverEverything: Boolean =
    tests.map(_.coverage).reduce(_ merge _).isComplete

  /** returns the minimal set of tests for maximal coverage so far */
  def testCollection: Set[GeneratedTest] =
    Set.empty[GeneratedTest] ++ tests
}
