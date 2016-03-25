package de.hpi.asg.breezetestgen.domain.components

import de.hpi.asg.breezetestgen.constraintsolving.ConstraintVariable
import de.hpi.asg.breezetestgen.domain.components.HandshakeComponent.{Reaction, State}
import de.hpi.asg.breezetestgen.domain.{DataAcknowledge, PullChannel, _}
import de.hpi.asg.breezetestgen.testgeneration.TestOp
import de.hpi.asg.breezetestgen.testing.TestEvent
import de.hpi.asg.breezetestgen.util.FSM

/** base class for the behaviour definiton of BrzComponents
  *
  * @param initState the state of the component to start from
  * @tparam C control state
  * @tparam D data state
  */
abstract class BrzComponentBehaviour[C, D] protected(initState: HandshakeComponent.State[C, D])
  extends FSM[C, D, (Signal, TestEvent)] {
  startWith(initState.controlState, initState.dataState)

  // reaction will be build up with helper methods during signal handling
  private[this] var reaction: Reaction = _

  /** holds the testEvent of current signal, if needed */
  protected var testEvent: TestEvent = _

  /** processes one step of fsm with given input */
  def handleSignal(s: Signal, te: TestEvent): Reaction = {
    reaction = Reaction.empty
    testEvent = te
    processMsg((s, te))
    reaction
  }

  /** returns complete current state of the FSM, which can be used for replicating the FSM */
  def state = State(myState.stateName, myState.stateData)

  private def addSignal(s: Signal) = reaction = reaction.addSignal(s)

  /** helper methods for signaling other components */
  protected def request(channelId: Channel.Spec[NoPushChannel[_]]) = addSignal(Request(channelId))
  protected def acknowledge(channelId: Channel.Spec[NoPullChannel[_]]) = addSignal(Acknowledge(channelId))
  protected def dataRequest(channelId: Channel.Spec[PushChannel[_]], data: Data) = addSignal(DataRequest(channelId, data))
  protected def dataAcknowledge(channelId: Channel.Spec[PullChannel[_]], data: Data) = addSignal(DataAcknowledge(channelId, data))

  /** helper method for constraint addition */
  protected def constrain(cv: ConstraintVariable) = reaction = reaction.addConstraint(cv)
  protected def constrain(cvs: Set[ConstraintVariable]) = reaction = reaction.addConstraints(cvs)
  /** helper method for setting testOp*/
  protected def testOp(op: TestOp) = reaction = reaction.setTestOp(op)


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
