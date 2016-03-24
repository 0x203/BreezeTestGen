package de.hpi.asg.breezetestgen

import de.hpi.asg.breezetestgen.domain.components.ComponentBehaviour
import de.hpi.asg.breezetestgen.domain._
import de.hpi.asg.breezetestgen.testing.{MergeEvent, TestEvent}
import ComponentBehaviour.Reaction
import de.hpi.asg.breezetestgen.domain.components.brzcomponents.Sequence


class SequenceSpec extends baseclasses.UnitTest {
  val activateId: Channel.Spec[SyncChannel[_]] = 1
  val outIds: Seq[Channel.Spec[SyncChannel[_]]] = Seq(2, 3, 4)
  val te: TestEvent = new MergeEvent()

  val sequence = new Sequence(id = 0, activateId, outIds)

  "A Sequence component" should "have a behaviour" in {
    val sequenceBehaviour = sequence.behaviour(None)
    assert(sequenceBehaviour.isInstanceOf[ComponentBehaviour[sequence.C, sequence.D]])
  }

  it should "respond to an request on activate with request first out" in {
    val sequenceBehaviour = sequence.behaviour(None)
    val activateRequest = Request(activateId)

    assertResult(Reaction(Set(Request(outIds.head)), None, Set.empty)) {
      sequenceBehaviour.handleSignal(activateRequest, te)
    }
  }

  it should "throw an UnhandledException when getting another signal first" in {
    val sequenceBehaviour = sequence.behaviour(None)
    val inpAcknowledge = Acknowledge(outIds.last)

    intercept[sequenceBehaviour.UnhandledException] {
      sequenceBehaviour.handleSignal(inpAcknowledge, te)
    }
  }

  it should "behave like BrzSequence would behave" in {
    val sequenceBehaviour = sequence.behaviour(None)

    val signals: Seq[Signal] = Request(activateId) +: outIds.map{id => Acknowledge(id)}
    val reactions: Seq[Reaction] = outIds.map{id => Reaction(Set(Request(id)), None, Set.empty)} :+
      Reaction(Set(Acknowledge(activateId)), None, Set.empty)

    for ((s, r) <- signals.zip(reactions))
      yield assertResult(r) {sequenceBehaviour.handleSignal(s, te)}
  }

  it should "be restorable from state" in {
    val firstSequenceB = sequence.behaviour(None)
    firstSequenceB.handleSignal(Request(activateId), te)

    val state = firstSequenceB.state

    val secondSequenceB = sequence.behaviour(Some(state))

    assertResult(Reaction(Set(Request(outIds(1))), None, Set.empty)) {
      secondSequenceB.handleSignal(Acknowledge(outIds.head), te)
    }
  }
}
