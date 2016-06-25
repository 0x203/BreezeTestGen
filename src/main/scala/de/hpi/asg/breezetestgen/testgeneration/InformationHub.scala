package de.hpi.asg.breezetestgen.testgeneration

import de.hpi.asg.breezetestgen.Loggable
import de.hpi.asg.breezetestgen.testgeneration.constraintsolving._
import de.hpi.asg.breezetestgen.actors.HandshakeActor
import de.hpi.asg.breezetestgen.actors.HandshakeActor.Decision
import de.hpi.asg.breezetestgen.domain.{DataPort, SyncPort, _}
import de.hpi.asg.breezetestgen.domain.components.BrzComponentBehaviour.{DecisionPossibilities, NormalFlowReaction}
import de.hpi.asg.breezetestgen.domain.components.HandshakeComponent
import de.hpi.asg.breezetestgen.testgeneration.TestGenerator.SleepingExecution
import de.hpi.asg.breezetestgen.testing.coverage.{ChannelActivationCoverage, Coverage}
import de.hpi.asg.breezetestgen.testing.{IOEvent, TestEvent}

/** Central place for gathering information according a single a test/simulation run.
  *
  * Gathers [[de.hpi.asg.breezetestgen.testing.TestEvent]]s in a [[TestBuilder]],
  * a [[constraintsolving.ConstraintCollection]] and (later) coverage statistics.
  *
  */
class InformationHub(private val runId: Int,
                     private val netlist: Netlist,
                     var cc: ConstraintCollection,
                     testBuilder: TestBuilder,
                     var coverage: Coverage,
                     var remainingLoopExecs: Int) extends Loggable{
  import InformationHub._
  /** records reaction from [[de.hpi.asg.breezetestgen.domain.components.BrzComponentBehaviour]]
    *
    * @param reaction reaction from handshake component
    * @param id hierarchical id of component which sent this
    * @return a [[TestEvent]] for further building of tests, if a [[TestOp]] was specified
    */
  def handleReaction(reaction: NormalFlowReaction,
                     id: HandshakeComponent.Id): Either[TestEvent , Option[GeneratedTest]] =
    if (enoughLoopExecutions(id)) {
      Right(None)
    } else
      Left(handleNormalReaction(reaction))


  private def handleNormalReaction(reaction: NormalFlowReaction): TestEvent = {
    reaction.signals
      .collect{case dataSignal: SignalWithData => dataSignal.data}
      .collect{case vd: VariableData => vd}
      .foreach(vd => {
        // TODO refactor this badly
        cc = cc.addVariable(vd.underlying)
        val cons = vd.constraint
        if (cons != null)
          cc = cc.addConstraint(cons)
      })

    coverage = coverage.withSignals(reaction.signals)

    reaction.testOp match {
      case Merge(te) => testBuilder.merge(te)
      case Follow(te) => te
    }
  }

  private val loopIds = netlist.loopIds
  def enoughLoopExecutions(id: HandshakeComponent.Id): Boolean = {
    if (loopIds contains id) {
      remainingLoopExecs -= 1
      info(s"Detected Loop execution! Remaining: $remainingLoopExecs")
      remainingLoopExecs < 0
    } else
      false
  }

  def handlePortSignal(signal: Signal, testEvent: TestEvent):
    Either[Seq[HandshakeActor.Signal], Option[GeneratedTest]] = {
    val successor = newIOEvent(signal, testEvent)

    if(signal == Signal.ActivateAcknowledge)
      // test finished, instantiate it
      Right(generateTest())
    else
      // answer with next signal
      Left(mirrorSignal(signal, successor))
  }

  /** returns the current state, maybe used for duplication or such things later */
  def state(): InformationHub.State = InformationHub.State(cc, testBuilder.duplicate(), coverage, remainingLoopExecs)

  /** returns the requests that should be sent initially*/
  def initialRequests(): Seq[HandshakeActor.Signal] =
    packSignals(initialRequestsForNetlist(netlist))

  /** creates a [[ConstraintCollection]] for each possibility that is feasible after this */
  def createSleepingExecutions(componentId: HandshakeComponent.Id,
                               possibilities: DecisionPossibilities): Map[ConstraintVariable, SleepingExecution] = {
    val newCCs = possibilities.keys
      .map{case constraint => constraint -> cc.fork().add(List(constraint))}
      .filter{case (_, newCC) => new ChocoSolver(newCC).isFeasible}
      .toMap[ConstraintVariable, ConstraintCollection]

    val remainingPossibilities = possibilities.filterKeys(newCCs.keySet contains _)

    remainingPossibilities.map { case (cv, (reaction, newState)) =>
      val newInformationHub = new InformationHub(runId, netlist, newCCs(cv), testBuilder, coverage, remainingLoopExecs)
      val testEventO = newInformationHub.handleNormalReaction(reaction)
      val decision = Decision(componentId, newState, reaction.signals, testEventO)
      cv -> SleepingExecution(newInformationHub.state(), decision)
    }
  }

  /** record IOEvent on the interface between main netlist and environment */
  private def newIOEvent(signal: Signal, testEvent: TestEvent): TestEvent = {
    //record coverage
    coverage = coverage.withSignal(signal)
    //create succeeding TestEvent
    testBuilder.addSuccessor(testEvent, IOEvent(signal))
  }

  /** reacts to a signal from the netlist with the counter-signal on the same port
    *
    * @param signal signal to be mirrored
    * @param testEvent predecessor of new one
    */
  private def mirrorSignal(signal: Signal, testEvent: TestEvent) = {
    val portId = channelIdToPortId(signal.channelId)
    val answerSignal = signalOnPort(netlist.ports(portId))
    val answerEvent = newIOEvent(answerSignal, testEvent)

    packSignals(List(answerSignal), Option(answerEvent))
  }
  private val channelIdToPortId = netlist.ports.values.map{p => p.channelId -> p.id}.toMap[Channel.Id, Port.Id]

  /** transform domain signals to HandshakeActor.Signals which can be send to the actors */
  private def packSignals(signals: Seq[Signal], teO: Option[TestEvent] = None) =
    signals
      .map{ds => HandshakeActor.Signal(runId, Nil, ds, teO getOrElse IOEvent(ds))} // create understandable signals

  /** returns a generated test if one exists */
  private def generateTest(): Option[GeneratedTest] =
    TestInstantiator.random(cc, testBuilder).map(GeneratedTest(_, coverage))
}

object InformationHub {
  case class State(constraintCollection: ConstraintCollection,
                   testBuilder: TestBuilder,
                   coverage: Coverage,
                   remainingLoopExecs: Int)

  def fromState(runId: Int, netlist: Netlist, state: State): InformationHub =
    new InformationHub(runId, netlist, state.constraintCollection, state.testBuilder, state.coverage,
      state.remainingLoopExecs)

  def forNetlist(runId: Int, netlist: Netlist, maxLoopExecs: Int): InformationHub = {
    val initialSignals = initialRequestsForNetlist(netlist).toSet
    new InformationHub(
      runId, netlist,
      ConstraintCollection(
        variables = initialSignals.collect{case DataRequest(_, vd :VariableData) => vd.underlying}
      ),
      TestBuilder.withOrigins(
        initialSignals.map(IOEvent(_))
      ),
      ChannelActivationCoverage.forNetlist(netlist).withSignals(initialSignals),
      maxLoopExecs
    )
  }

  private def initialRequestsForNetlist(netlist: Netlist): Seq[Signal] = {
    netlist.activePorts.map(signalOnPort).toSeq.sortBy(_.channelId)
  }

  private def signalOnPort(port: Port): Signal = port match {
    case sp: SyncPort => sp.createSignal()
    case dp: DataPort =>
      val v = new Variable(dp.name, dp.bitCount, dp.isSigned)
      val d = new VariableData(v, null)
      dp.createSignal(d)
  }
}
