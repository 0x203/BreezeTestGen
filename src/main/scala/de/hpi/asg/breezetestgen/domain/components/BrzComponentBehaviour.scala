package de.hpi.asg.breezetestgen.domain.components

import de.hpi.asg.breezetestgen.domain.{DataAcknowledge, PullChannel, _}
import de.hpi.asg.breezetestgen.testgeneration.{Follow, Merge, TestOp, constraintsolving}
import constraintsolving.{Constraint, ConstraintVariable}
import de.hpi.asg.breezetestgen.testing.TestEvent
import de.hpi.asg.breezetestgen.util.FSM

/** base class for the behaviour definition of BrzComponents
  *
  * @param initState the state of the component to start from
  * @tparam C control state
  * @tparam D data state
  */
abstract class BrzComponentBehaviour[C, D] protected(initState: HandshakeComponent.State[C, D])
  extends FSM[C, D, (Signal, TestEvent)] {
  import BrzComponentBehaviour._

  startWith(initState.controlState, initState.dataState)

  // normalFlowReaction will be build up with helper methods during signal handling
  private[this] var normalFlowReaction: NormalFlowReaction = _

  // if needed, this will be set and returned
  private[this] var decisionRequired: Option[DecisionRequired] = None

  /** holds the testEvent of current signal, if needed */
  protected var testEvent: TestEvent = _

  /** processes one step of fsm with given input */
  def handleSignal(s: Signal, te: TestEvent): Reaction = {
    normalFlowReaction = NormalFlowReaction.afterTestEvent(te)
    decisionRequired = None
    testEvent = te
    processMsg((s, te))
    decisionRequired getOrElse normalFlowReaction

  }

  /** returns complete current state of the FSM, which can be used for replicating the FSM */
  def state: HandshakeComponent.State[C, D] = HandshakeComponent.State(currentState.stateName, currentState.stateData)
  /** sets the current state of the FSM */
  def state_=(state: HandshakeComponent.State[_, _]) =
    currentState = FSM.State(state.controlState.asInstanceOf[C], state.dataState.asInstanceOf[D])

  /** determines possible reactions and set DecisionRequired reaction, if needed */
  protected def decideBetween(possibilities: Map[Data.ConstraintOrBool, () => State]): Option[State] = {
    possibilities.collectFirst{
      case (Right(true), f) => f  // first, check if there is any path that's definitely it
    }.map(_.apply())
      .orElse {
        // else, we have to create a DecisionRequest reaction
        createDecisionRequest(
          // paths that are definitely false can be filtered already, just constraints matter
          possibilities.collect { case (Left(c), f) => c -> f }
        )
        // then, we cannot tell which state should be transferred to
        None
      }
  }

  private def createDecisionRequest(possibilities: Map[Constraint, () => State]) = {
    val currentReactionStatus = normalFlowReaction  // save it for fresh state for each possibility

    val eachReaction: DecisionPossibilities =
      // execute function for each possibility
      possibilities.map { case (c, f) =>
        // reset normalFlowState
        normalFlowReaction = currentReactionStatus
        // run function, capture final state
        val state = f.apply()
        // save modified reaction and resulting state
        c -> (normalFlowReaction, state)
      }.map{
        // map it to the right types
        case (c, (nfr, s)) =>
          ConstraintVariable.constraint2ConstraintCV(c) -> (nfr, HandshakeComponent.State(s.stateName, s.stateData))
      }

    // set variable to return it later
    decisionRequired = Option(DecisionRequired(eachReaction))
  }

  private def addSignal(s: Signal) = normalFlowReaction = normalFlowReaction.addSignal(s)

  /** helper methods for signaling other components */
  protected def request(channelId: Channel.Spec[NoPushChannel[_]]) = addSignal(Request(channelId))
  protected def acknowledge(channelId: Channel.Spec[NoPullChannel[_]]) = addSignal(Acknowledge(channelId))
  protected def dataRequest(channelId: Channel.Spec[PushChannel[_]], data: Data) = addSignal(DataRequest(channelId, data))
  protected def dataAcknowledge(channelId: Channel.Spec[PullChannel[_]], data: Data) = addSignal(DataAcknowledge(channelId, data))

  /** helper method for setting testOp*/
  protected def mergeAfter(tes: Set[TestEvent]) = normalFlowReaction = normalFlowReaction.copy(testOp = Merge(tes))


  // Message Event Extractors
  object Req {
    def unapply(e: Event): Option[(Channel.Spec[NoPushChannel[_]], D)] = {
      e match {
        case FSM.Event((Request(c), _), i) => Some(c, i)
        case _ => None
      }
    }
  }
  object Ack {
    def unapply(e: Event): Option[(Channel.Spec[NoPullChannel[_]], D)] = {
      e match {
        case FSM.Event((Acknowledge(c), _), i) => Some(c, i)
        case _ => None
      }
    }
  }
  object DataReq {
    def unapply(e: Event): Option[(Channel.Spec[PushChannel[_]], Data, D)] = {
      e match {
        case FSM.Event((DataRequest(c, d), _), i) => Some(c, d, i)
        case _ => None
      }
    }
  }
  object DataAck {
    def unapply(e: Event): Option[(Channel.Spec[PullChannel[_]], Data, D)] = {
      e match {
        case FSM.Event((DataAcknowledge(c, d), _), i) => Some(c, d, i)
        case _ => None
      }
    }
  }

  type UnhandledException = FSM.UnhandledException[Signal, D]
}

object BrzComponentBehaviour {
  type DecisionPossibilities = Map[ConstraintVariable, (NormalFlowReaction, HandshakeComponent.State[_, _])]

  sealed trait Reaction

  case class DecisionRequired(possibilities: DecisionPossibilities)
    extends Reaction

    case class NormalFlowReaction(signals: Set[Signal], testOp: TestOp) extends Reaction {
      def addSignal(s: Signal): NormalFlowReaction = copy(signals = signals + s)
  }
  object NormalFlowReaction {
    def afterTestEvent(te: TestEvent): NormalFlowReaction = NormalFlowReaction(Set.empty, Follow(te))
  }
}
