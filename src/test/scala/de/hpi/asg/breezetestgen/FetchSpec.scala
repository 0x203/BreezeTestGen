package de.hpi.asg.breezetestgen

import baseclasses.UnitTest
import de.hpi.asg.breezetestgen.testing.{MergeEvent, TestEvent}
import de.hpi.asg.breezetestgen.domain._
import components.BrzComponentBehaviour
import de.hpi.asg.breezetestgen.domain.components.HandshakeComponent.Reaction
import de.hpi.asg.breezetestgen.domain.components.brzcomponents.Fetch

class FetchSpec extends UnitTest {
  val activateId: Channel.Spec[SyncChannel[_]] = 1
  val inpId: Channel.Spec[PullChannel[_]] = 2
  val outId: Channel.Spec[PushChannel[_]] = 3
  val te: TestEvent = new MergeEvent()

  val fetch = new Fetch(id = 0, activateId, inpId, outId)

  "A Fetch component" should "have a behaviour" in {
    val fetchBehaviour = fetch.behaviour(None)
    assert(fetchBehaviour.isInstanceOf[BrzComponentBehaviour[fetch.C, fetch.D]])
  }

  it should "respond to an request on activate with another request on inp" in {
    val fetchBehaviour = fetch.behaviour(None)
    val activateRequest = Request(activateId)

    assertResult(Reaction(Set(Request(inpId)), None, Set.empty)) {
      fetchBehaviour.handleSignal(activateRequest, te)
    }
  }

  it should "throw an UnhandledException when getting another signal first" in {
    val fetchBehaviour = fetch.behaviour(None)
    val inpAcknowledge = Acknowledge(inpId)

    intercept[fetchBehaviour.UnhandledException] {
      fetchBehaviour.handleSignal(inpAcknowledge, te)
    }
  }

  it should "behave like BrzFetch would behave" in {
    val fetchBehaviour = fetch.behaviour(None)
    val activateRequest = Request(activateId)

    assertResult(Reaction(Set(Request(inpId)), None, Set.empty)) {
      fetchBehaviour.handleSignal(activateRequest, te)
    }

    val sampleData = Constant(5)
    val inpAck = DataAcknowledge(inpId, sampleData)
    //TODO: put something into ConstraintsNVariables Set if it will be defined
    assertResult(Reaction(Set(DataRequest(outId, sampleData)), None, Set.empty)) {
      fetchBehaviour.handleSignal(inpAck, te)
    }

    val outAck = Acknowledge(outId)
    assertResult(Reaction(Set(Acknowledge(activateId)), None, Set.empty)) {
      fetchBehaviour.handleSignal(outAck, te)
    }
  }

  it should "be restorable from state" in {
    val firstFetchB = fetch.behaviour(None)
    firstFetchB.handleSignal(Request(activateId), te)

    val state = firstFetchB.state

    val secondFetchB = fetch.behaviour(Some(state))

    val sampleData = Constant(5)
    //TODO: put something into ConstraintsNVariables Set if it will be defined
    assertResult(Reaction(Set(DataRequest(outId, sampleData)), None, Set.empty)) {
      secondFetchB.handleSignal(DataAcknowledge(inpId, sampleData), te)
    }
  }
}
