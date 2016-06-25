package de.hpi.asg.breezetestgen

import baseclasses.UnitTest
import de.hpi.asg.breezetestgen.domain.{Constant, DataAcknowledge, Request, Signal}
import de.hpi.asg.breezetestgen.fixtures.GCDTest
import de.hpi.asg.breezetestgen.testing.{IOEvent, JsonFromTo}

class TestJsonSpec extends UnitTest {
  private val (ain, bin, o) = (12, 8, 4)
  private val gcdTest = GCDTest(ain, bin, o)

  def isDefinedIn(signal: Signal, test: testing.Test) = {
    test.nodes.map(_.value).collectFirst{case IOEvent(_, `signal`) => true}.isDefined
  }

  "JSON export and import" should "produce the same test again" in {
    val gcdJson = JsonFromTo.toJson(gcdTest)
    val gcdAgainO = JsonFromTo.fromJson(gcdJson)
    assert(gcdAgainO.isDefined)
    val gcdAgain = gcdAgainO.get

    // okay, this could be tested better
    assert(isDefinedIn(Request(2), gcdAgain))
    assert(isDefinedIn(DataAcknowledge(2, Constant(ain)), gcdAgain))
  }
}
