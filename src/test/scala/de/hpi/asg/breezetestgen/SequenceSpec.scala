package de.hpi.asg.breezetestgen

import de.hpi.asg.breezetestgen.testing.{MergeEvent, TestEvent}
import de.hpi.asg.breezetestgen.domain._
import de.hpi.asg.breezetestgen.domain.components.BrzComponentBehaviour
import de.hpi.asg.breezetestgen.domain.components.BrzComponentBehaviour.NormalFlowReaction
import de.hpi.asg.breezetestgen.domain.components.brzcomponents.Sequence
import de.hpi.asg.breezetestgen.testgeneration.Follow


class SequenceSpec extends baseclasses.UnitTest {
  val activateId: Channel.Spec[SyncChannel[_]] = 1
  val outIds: Seq[Channel.Spec[SyncChannel[_]]] = Seq(2, 3, 4)
  val te: TestEvent = TestEvent.newMergeEvent()

  val sequence = new Sequence(id = -1 :: 0 :: Nil, activateId, outIds)

  def normalReactionWith(ds: Signal): NormalFlowReaction =
    NormalFlowReaction(Set(ds), Follow(te))

  "A Sequence component" should "have a behaviour" in {
    val sequenceBehaviour = sequence.behaviour(None)
    assert(sequenceBehaviour.isInstanceOf[BrzComponentBehaviour[sequence.C, sequence.D]])
  }

  it should "respond to an request on activate with request first out" in {
    val sequenceBehaviour = sequence.behaviour(None)
    val activateRequest = Request(activateId)

    assertResult(normalReactionWith(Request(outIds.head))) {
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
    val reactions: Seq[NormalFlowReaction] = outIds.map{id => normalReactionWith(Request(id))} :+
      normalReactionWith(Acknowledge(activateId))

    for ((s, r) <- signals.zip(reactions))
      yield assertResult(r) {sequenceBehaviour.handleSignal(s, te)}
  }

  it should "be restorable from state" in {
    val firstSequenceB = sequence.behaviour(None)
    firstSequenceB.handleSignal(Request(activateId), te)

    val state = firstSequenceB.state

    val secondSequenceB = sequence.behaviour(Some(state))

    assertResult(normalReactionWith(Request(outIds(1)))) {
      secondSequenceB.handleSignal(Acknowledge(outIds.head), te)
    }
  }
}
