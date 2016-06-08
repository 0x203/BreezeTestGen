package de.hpi.asg.breezetestgen

import baseclasses.UnitTest
import de.hpi.asg.breezetestgen.domain.{Constant, DataAcknowledge, Request}
import de.hpi.asg.breezetestgen.fixtures.GCDTest
import de.hpi.asg.breezetestgen.testing.{IOEvent, JsonFromTo}

class TestJsonSpec extends UnitTest {
  private val (ain, bin, o) = (12, 8, 4)
  private val gcdTest = GCDTest(ain, bin, o)

  "JSON export and import" should "produce the same test again" in {
    val gcdJson = JsonFromTo.toJson(gcdTest)
    val gcdAgainO = JsonFromTo.fromJson(gcdJson)
    assert(gcdAgainO.isDefined)
    val gcdAgain = gcdAgainO.get

    // okay, this could be tested better
    assert(gcdAgain.find(IOEvent(Request(2))).isDefined)
    assert(gcdAgain.find(IOEvent(DataAcknowledge(2,Constant(ain)))).isDefined)
  }
}
