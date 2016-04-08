package de.hpi.asg.breezetestgen

import de.hpi.asg.breezetestgen.baseclasses.UnitTest
import de.hpi.asg.breezetestgen.domain.{Acknowledge, Constant, DataAcknowledge, Request}
import de.hpi.asg.breezetestgen.domain.components.BrzComponentBehaviour
import de.hpi.asg.breezetestgen.domain.components.BrzComponentBehaviour.{DecisionRequired, NormalFlowReaction}
import de.hpi.asg.breezetestgen.domain.components.HandshakeComponent.State
import de.hpi.asg.breezetestgen.domain.components.brzcomponents.While
import de.hpi.asg.breezetestgen.testing.{MergeEvent, TestEvent}
import de.hpi.asg.breezetestgen.testgeneration.{Follow, VariableData, constraintsolving}


class WhileSpec extends UnitTest {
  val activateId = 1
  val guardId = 3
  val outId = 7

  val te: TestEvent = new MergeEvent()

  def normalReactionWith(ds: domain.Signal): NormalFlowReaction =
    NormalFlowReaction(Set(ds), Follow(te), Set.empty)

  val brzWhile = new While(id = 0, activateId, guardId, outId)
  val evaluatingState: State[brzWhile.C, brzWhile.D] = State(brzWhile.WhileBehaviour.Evaluating, null)

  val noEnterReaction = normalReactionWith(Acknowledge(activateId))
  val enterReaction = normalReactionWith(Request(outId))

  def createBehaviour(state: Option[State[brzWhile.C, brzWhile.D]]): BrzComponentBehaviour[_, _] =
    brzWhile.behaviour(state)

  "The WhileBehaviour" should "correctly go to evaluating state if requested" in {
    val whileBehaviour = createBehaviour(None)

    assertResult(normalReactionWith(Request(guardId))) {
      whileBehaviour.handleSignal(Request(activateId), te)
    }

    assert(whileBehaviour.state == evaluatingState)
  }

  it should "not enter loop if guard is false" in {
    val whileBehaviour = createBehaviour(Some(evaluatingState))
    val falseConst = Constant(0, 1)

    assertResult(noEnterReaction) {
      whileBehaviour.handleSignal(DataAcknowledge(guardId, falseConst), te)
    }
  }

  it should "enter loop if guard is true" in {
    val whileBehaviour = createBehaviour(Some(evaluatingState))
    val trueConst = Constant(1, 1)

    assertResult(enterReaction) {
      whileBehaviour.handleSignal(DataAcknowledge(guardId, trueConst), te)
    }
  }

  it should "return DecisionRequired if guard is DataVariable" in {
    val whileBehaviour = createBehaviour(Some(evaluatingState))
    val boolVariable = constraintsolving.BoolVariable("guard")
    val guardVar = new VariableData(boolVariable, null)

    val reaction = whileBehaviour.handleSignal(DataAcknowledge(guardId, guardVar), te)

    assert(reaction.isInstanceOf[DecisionRequired])
    val possibilities = reaction.asInstanceOf[DecisionRequired].possibilities

    assert(possibilities.size == 2)

    val noEnterConstraint = boolVariable.isFalse
    assert(possibilities.isDefinedAt(noEnterConstraint))
    val noEnter = possibilities(noEnterConstraint)
    assert(noEnter._1 == noEnterReaction)
    assert(noEnter._2 == State(brzWhile.WhileBehaviour.Idle, null))

    val enterConstraint = boolVariable.isTrue
    assert(possibilities.isDefinedAt(enterConstraint))
    val enter = possibilities(enterConstraint)
    assert(enter._1 == enterReaction)
    assert(enter._2 == State(brzWhile.WhileBehaviour.Executing, null))
  }
}
