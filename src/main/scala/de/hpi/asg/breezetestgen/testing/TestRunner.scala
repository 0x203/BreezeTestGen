package de.hpi.asg.breezetestgen.testing

import de.hpi.asg.breezetestgen.domain.Signal

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.mutable.Graph

object TestRunner {
  private type RunningTest = Graph[TestEvent, DiEdge]

  sealed trait TestReaction
  case class EmitSignals(signals: Set[Signal]) extends TestReaction

  sealed trait TestResult extends TestReaction
  case object TestSucceeded extends TestResult
  case class TestFailed(unexpectedSignal: Signal, currentTest: Graph[TestEvent, DiEdge]) extends TestResult
}

class TestRunner(test: Test, shouldEmit: Signal => Boolean) {
  import TestRunner._

  /** should be called for first signals to emit when the test is fresh */
  def firstAction(): TestReaction = reaction()

  /** react to a signal emitted by the UUT */
  def reactToSignal(signal: Signal): TestReaction = {
    pendingEvents.find{case ioEvent: IOEvent => ioEvent.signal == signal} match {
      case Some(event: IOEvent) =>
        runningTest -= event
        reaction()
      case None => TestFailed(signal, runningTest)
    }
  }

  private def reaction(): TestReaction = {
    val toBeEmitted = processEventsToEmit()
    if (isDone)
      TestSucceeded
    else
      EmitSignals(toBeEmitted)
  }

  private val runningTest: RunningTest = Graph[TestEvent, DiEdge]() ++ test
  private def pendingEvents = runningTest.nodes.withFilter{!_.hasPredecessors}.map(_.value).toSet

  private def isDone: Boolean = runningTest.isEmpty

  private def noIncomingEvent(t: TestEvent): Boolean = t match {
    case m: MergeEvent => true
    case i: IOEvent => shouldEmit(i.signal)
  }

  /** removes all [[TestEvent]]s which are either [[MergeEvent]]s (no action) or outgoing [[IOEvent]]s
    * @return set of signals to emit
    */
  private def processEventsToEmit(): Set[Signal] = {
    import scalax.collection.GraphPredef.seqToGraphParam

    val eventsToRemove = pendingEvents  // from all pending events
      .withFilter(noIncomingEvent)      // filter those which we are expecting, not generating (i.e. an ack for our req)
      .map(runningTest.get)             // get InnerNodes (needed for n
      .flatMap { _.withSubgraph(nodes = noIncomingEvent(_)).toIterable }  // get all connected nodes which are not incoming
      .map(_.value)                     // map to OuterNodes again (i.e. TestEvents)

    // remove from our running test
    runningTest --= seqToGraphParam(eventsToRemove.toSeq)

    // map the remaining IOEvents to their corresponding signals
    eventsToRemove.collect { case ioe: IOEvent => ioe.signal }
  }
}
