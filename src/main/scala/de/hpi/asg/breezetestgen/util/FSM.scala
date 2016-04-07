package de.hpi.asg.breezetestgen.util

// the code in this file is heavily based on Akka actors FSM, but without being an actor and other features
// https://github.com/akka/akka/blob/master/akka-actor/src/main/scala/akka/actor/FSM.scala

object FSM {

  case class State[S, D](stateName: S, stateData: D) {
    /**
      * Modify state transition descriptor with new state data. The data will be
      * set when transitioning to the new state.
      */
    def using(nextStateData: D): State[S, D] = {
      copy(stateData = nextStateData)
    }
  }

  final case class Event[M, D](event: M, stateData: D)

  /** This will be thrown, if no handler is specified for incomming message */
  case class UnhandledException[M, D](msg: M, state: D)  extends Exception
}

/** Finite state machine with both control and data state (thus forming kind of an extended finite state machine)
  *
  * @tparam S control state, used to differentiate handlers using `when` calls
  * @tparam D data state
  * @tparam M input alphabet
  */
class FSM[S, D, M] {
  import FSM._

  type State = FSM.State[S, D]
  type Event = FSM.Event[M, D]
  type StateFunction = scala.PartialFunction[Event, State]

  /**
    * Insert a new StateFunction at the end of the processing chain for the
    * given state.
    *
    * @param stateName designator for the state
    * @param stateFunction partial function describing response to input
    */
  final def when(stateName: S)(stateFunction: StateFunction): Unit =
    register(stateName, stateFunction)

  /**
    * Set handler which is called upon reception of unhandled messages. Calling
    * this method again will overwrite the previous contents.
    *
    * The current state may be queried using ``stateName``.
    */
  final def whenUnhandled(stateFunction: StateFunction): Unit =
    handleEvent = stateFunction orElse handleEventDefault

  /**
    * Set initial state. Call this method from the constructor before the initialize method.
    *
    * @param stateName initial state designator
    * @param stateData initial state data
    */
  final def startWith(stateName: S, stateData: D): Unit =
    currentState = FSM.State(stateName, stateData)

  /**
    * Produce transition to other state.
    * Return this from a state function in order to effect the transition.
    *
    * @param nextStateName state designator for the next state
    * @return state transition descriptor
    */
  final def goto(nextStateName: S): State = FSM.State(nextStateName, currentState.stateData)

  /**
    * Produce "empty" transition descriptor.
    * Return this from a state function when no state change is to be effected.
    *
    * @return descriptor for staying in current state
    */
  final def stay: State = goto(currentState.stateName)

  /**
    * Verify existence of initial state and setup timers. This should be the
    * last call within the constructor.
    */
  final def initialize(): Unit = makeTransition(currentState)

  protected var currentState: State = _

  private val stateFunctions = collection.mutable.Map[S, StateFunction]()

  private def register(name: S, function: StateFunction): Unit = {
    if (stateFunctions contains name) {
      stateFunctions(name) = stateFunctions(name) orElse function
    } else {
      stateFunctions(name) = function
    }
  }

  private val handleEventDefault: StateFunction = {
    case Event(value, stateData) =>
      throw new UnhandledException[M, D](value, stateData)
  }
  private var handleEvent: StateFunction = handleEventDefault

  protected def processMsg(msg: M): Unit = {
    val event = Event(msg, currentState.stateData)
    processEvent(event)
  }

  private def processEvent(event: Event): Unit = {
    val stateFunc = stateFunctions(currentState.stateName)
    val nextState = if (stateFunc isDefinedAt event) {
      stateFunc(event)
    } else {
      // handleEventDefault ensures that this is always defined
      handleEvent(event)
    }
    makeTransition(nextState)
  }

  private def makeTransition(nextState: State): Unit = {
    if (!stateFunctions.contains(nextState.stateName)) {
      throw new RuntimeException("Next state %s does not exist".format(nextState.stateName))
    } else {
      currentState = nextState
    }
  }

}
