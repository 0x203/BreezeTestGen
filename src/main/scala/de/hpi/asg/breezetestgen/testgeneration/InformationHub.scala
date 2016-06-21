package de.hpi.asg.breezetestgen.testgeneration

import de.hpi.asg.breezetestgen.actors.ComponentActor.Decision
import de.hpi.asg.breezetestgen.testgeneration.constraintsolving._
import de.hpi.asg.breezetestgen.actors.HandshakeActor
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
class InformationHub(private val runId: Netlist.Id,
                     private val netlist: Netlist,
                     var cc: ConstraintCollection,
                     testBuilder: TestBuilder,
                     var coverage: Coverage) {
  import InformationHub._

  /** records reaction from [[de.hpi.asg.breezetestgen.domain.components.BrzComponentBehaviour]]
    *
    * @param reaction reaction from handshake component
    * @return a [[TestEvent]] for further building of tests, if a [[TestOp]] was specified
    */
  def handleReaction(reaction: NormalFlowReaction): TestEvent = {
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

  def handlePortSignal(signal: Signal, testEvent: TestEvent):
    Either[Seq[HandshakeActor.Signal], Option[GeneratedTest]] = {
    //record coverage
    coverage = coverage.withSignal(signal)
    //create succeeding TestEvent
    val successor = testBuilder.addSuccessor(testEvent, IOEvent(signal))

    if(signal == Signal.ActivateAcknowledge)
      // test finished, instantiate it
      Right(generateTest())
    else
      // answer with next signal
      Left(mirrorSignal(signal, successor))
  }

  /** returns the current state, maybe used for duplication or such things later */
  def state(): InformationHub.State = (cc, testBuilder.duplicate(), coverage)

  /** returns the requests that should be sent initially*/
  def initialRequests(): Seq[HandshakeActor.Signal] =
    packSignals(runId, initialRequestsForNetlist(netlist))

  /** creates a [[ConstraintCollection]] for each possibility that is feasible after this */
  def createSleepingExecutions(idChain: List[Netlist.Id],
                               componentId: HandshakeComponent.Id,
                               possibilities: DecisionPossibilities): Map[ConstraintVariable, SleepingExecution] = {
    val newCCs = possibilities.keys
      .map{case constraint => constraint -> cc.fork().add(List(constraint))}
      .filter{case (_, newCC) => new ChocoSolver(newCC).isFeasible}
      .toMap[ConstraintVariable, ConstraintCollection]

    val remainingPossibilities = possibilities.filterKeys(newCCs.keySet contains _)

    remainingPossibilities.map { case (cv, (reaction, newState)) =>
      val newInformationHub = new InformationHub(runId, netlist, newCCs(cv), testBuilder, coverage)
      val testEventO = newInformationHub.handleReaction(reaction)
      val decision = Decision(idChain, componentId, newState, reaction.signals, testEventO)
      cv -> SleepingExecution(newInformationHub.state(), decision)
    }
  }

  /** reacts to a signal from the netlist with the counter-signal on the same port
    *
    * @param signal signal to be mirrored
    * @param testEvent predecessor of new one
    */
  private def mirrorSignal(signal: Signal, testEvent: TestEvent) = {
    val portId = channelIdToPortId(signal.channelId)
    val answerSignal = signalOnPort(netlist.ports(portId))

    packSignals(runId, List(answerSignal), Option(testEvent))
  }
  private val channelIdToPortId = netlist.ports.values.map{p => p.channelId -> p.id}.toMap[Channel.Id, Port.Id]

  /** transform domain signals to HandshakeActor.Signals which can be send to the actors */
  private def packSignals(runId: Netlist.Id, signals: Seq[Signal], teO: Option[TestEvent] = None) =
    signals
      .map{ds => HandshakeActor.Signal(runId :: Nil, ds, teO getOrElse IOEvent(ds))} // create understandable signals

  /** returns a generated test if one exists */
  private def generateTest(): Option[GeneratedTest] =
    TestInstantiator.random(cc, testBuilder).map(GeneratedTest(_, coverage))
}

object InformationHub {
  type State = (ConstraintCollection, TestBuilder, Coverage)

  def fromState(runId: Netlist.Id, netlist: Netlist, state: State): InformationHub =
    new InformationHub(runId, netlist, state._1, state._2, state._3)

  def forNetlist(runId: Netlist.Id, netlist: Netlist): InformationHub = {
    val initialSignals = initialRequestsForNetlist(netlist).toSet
    new InformationHub(
      runId, netlist,
      ConstraintCollection(
        variables = initialSignals.collect{case DataRequest(_, vd :VariableData) => vd.underlying}
      ),
      TestBuilder.withOrigins(
        initialSignals.map(IOEvent(_))
      ),
      ChannelActivationCoverage.forNetlist(netlist).withSignals(initialSignals)
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
