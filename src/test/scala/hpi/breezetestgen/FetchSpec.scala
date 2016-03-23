package de.hpi.breezetestgen

import baseclasses.UnitTest
import de.hpi.asg.breezetestgen.domain
import de.hpi.asg.breezetestgen.domain.ComponentBehaviour.Reaction
import de.hpi.asg.breezetestgen.domain._
import de.hpi.asg.breezetestgen.domain.components.Fetch

class FetchSpec extends UnitTest {
  case class Foo(i: Int) extends domain.Data {
    type Type = Int
  }

  val activateId: Channel.Spec[SyncChannel[_]] = 1
  val inpId: Channel.Spec[PullChannel[_]] = 2
  val outId: Channel.Spec[PushChannel[_]] = 3
  val fetch = new Fetch[Foo](activateId, inpId, outId)

  "A Fetch component" should "have a behaviour" in {
    val fetchBehaviour = fetch.behaviour(None)
    assert(fetchBehaviour.isInstanceOf[ComponentBehaviour[Foo, fetch.C, fetch.D]])
  }

  it should "respond to an request on activate with another request on inp" in {
    val fetchBehaviour = fetch.behaviour(None)
    val activateRequest = Request(activateId)

    assertResult(Reaction[Foo](Set(Request(inpId)), None, Set.empty)) {
      fetchBehaviour.handleSignal(activateRequest)
    }
  }

  it should "throw an UnhandledException when getting another signal first" in {
    val fetchBehaviour = fetch.behaviour(None)
    val inpAcknowledge = Acknowledge(inpId)

    intercept[fetchBehaviour.UnhandledException] {
      fetchBehaviour.handleSignal(inpAcknowledge)
    }
  }

  it should "behave like BrzFetch would behave" in {
    val fetchBehaviour = fetch.behaviour(None)
    val activateRequest = Request(activateId)

    assertResult(Reaction[Foo](Set(Request(inpId)), None, Set.empty)) {
      fetchBehaviour.handleSignal(activateRequest)
    }

    val sampleData = Foo(5)
    val inpAck = DataAcknowledge(inpId, sampleData)
    //TODO: put something into ConstraintsNVariables Set if it will be defined
    assertResult(Reaction[Foo](Set(DataRequest(outId, sampleData)), None, Set.empty)) {
      fetchBehaviour.handleSignal(inpAck)
    }

    val outAck = Acknowledge(outId)
    assertResult(Reaction[Foo](Set(Acknowledge(activateId)), None, Set.empty)) {
      fetchBehaviour.handleSignal(outAck)
    }
  }

  it should "be restorable from state" in {
    val firstFetchB = fetch.behaviour(None)
    firstFetchB.handleSignal(Request(activateId))

    val state = firstFetchB.state

    val secondFetchB = fetch.behaviour(Some(state))

    val sampleData = Foo(5)
    //TODO: put something into ConstraintsNVariables Set if it will be defined
    assertResult(Reaction[Foo](Set(DataRequest(outId, sampleData)), None, Set.empty)) {
      secondFetchB.handleSignal(DataAcknowledge(inpId, sampleData))
    }
  }
}
