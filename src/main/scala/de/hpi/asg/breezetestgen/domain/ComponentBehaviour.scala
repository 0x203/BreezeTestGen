package de.hpi.asg.breezetestgen.domain

import de.hpi.asg.breezetestgen.util.FSM

object ComponentBehaviour {
  type TestOp = Int // TODO replace this with correct one
  type ConstraintsNVariables = Int // TODO replace this with correct one

  case class Reaction(signals: Set[Signal], te: Option[TestOp], cvs: Set[ConstraintsNVariables]) {
    def addSignal(s: Signal): Reaction = copy(signals = signals + s)
    def setTestOp(op: TestOp):Reaction = copy(te = Option(op))
    def addConstraint(cv: ConstraintsNVariables): Reaction = copy(cvs = cvs + cv)
    def addConstraints(new_cvs: Traversable[ConstraintsNVariables]): Reaction = copy(cvs = cvs ++ new_cvs)
  }
  object Reaction {
    def empty[DT <: Data]: Reaction = Reaction(Set.empty, None, Set.empty)
  }
}

/** base class for the behaviour definiton of BrzComponents
  *
  * @param initState the state of the component to start from
  * @tparam C control state
  * @tparam D data state
  */
abstract class ComponentBehaviour[C, D] protected(initState: ComponentState[C, D])
  extends FSM[C, D, Signal] {
  import ComponentBehaviour._
  startWith(initState.controlState, initState.dataState)

  // reaction will be build up with helper methods during signal handling
  private[this] var reaction: Reaction = _

  def handleSignal(s: Signal): Reaction = {
    reaction = Reaction.empty
    processMsg(s)
    reaction
  }

  def state = ComponentState(myState.stateName, myState.stateData)

  private def addSignal(s: Signal) = reaction = reaction.addSignal(s)

  protected def request(channelId: Channel.Spec[NoPushChannel[_]]) = addSignal(Request(channelId))
  protected def acknowledge(channelId: Channel.Spec[NoPullChannel[_]]) = addSignal(Acknowledge(channelId))
  protected def dataRequest(channelId: Channel.Spec[PushChannel[_]], data: Data) = addSignal(DataRequest(channelId, data))
  protected def dataAcknowledge(channelId: Channel.Spec[PullChannel[_]], data: Data) = addSignal(DataAcknowledge(channelId, data))

  protected def testOp(op: TestOp) = reaction = reaction.setTestOp(op)
  protected def constrain(cv: ConstraintsNVariables*) = reaction = reaction.addConstraints(cv)


  // Message Event Extractors
  object Req {
    def unapply(e: Event): Option[(Channel.Spec[NoPushChannel[_]], D)] = {
      e match {
        case FSM.Event(Request(c), i) => Some(c, i)
        case _ => None
      }
    }
  }
  object Ack {
    def unapply(e: Event): Option[(Channel.Spec[NoPullChannel[_]], D)] = {
      e match {
        case FSM.Event(Acknowledge(c), i) => Some(c, i)
        case _ => None
      }
    }
  }
  object DataReq {
    def unapply(e: Event): Option[(Channel.Spec[PushChannel[_]], Data, D)] = {
      e match {
        case FSM.Event(DataRequest(c, d), i) => Some(c, d, i)
        case _ => None
      }
    }
  }
  object DataAck {
    def unapply(e: Event): Option[(Channel.Spec[PullChannel[_]], Data, D)] = {
      e match {
        case FSM.Event(DataAcknowledge(c, d), i) => Some(c, d, i)
        case _ => None
      }
    }
  }

  type UnhandledException = FSM.UnhandledException[Signal, D]
}
