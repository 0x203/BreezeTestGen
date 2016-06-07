package de.hpi.asg.breezetestgen

import baseclasses.UnitTest
import de.hpi.asg.breezetestgen.fixtures.GCDTest

import de.hpi.asg.breezetestgen.testing.JsonFromTo

class TestJsonSpec extends UnitTest {
  private val (ain, bin, o) = (12, 8, 4)
  private val gcdTest = GCDTest(ain, bin, o)

  "JSON export" should "produce a correct JSON string" in {
    val gcdJson = JsonFromTo.toJson(gcdTest)
    //TODO: implement me correctly!
    assert(true)
  }
}
