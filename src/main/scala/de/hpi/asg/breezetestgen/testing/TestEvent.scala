package de.hpi.asg.breezetestgen.testing

import de.hpi.asg.breezetestgen.domain.{Constant, DataPort, SyncPort, Port}
import scalax.collection.GraphEdge.DiEdge


sealed trait TestEvent {
  //TODO: remove this when next version of graph library is released
  def ~>(other: TestEvent): DiEdge[TestEvent] = DiEdge(this, other)
}

class MergeEvent() extends TestEvent

trait IOEvent[P <: Port] extends TestEvent {
  val port: P
}
case class IOSyncEvent(port: SyncPort) extends IOEvent[SyncPort]
case class IODataEvent(port: DataPort, value: Constant) extends IOEvent[DataPort]

//TODO: differentiate between req und ack events (or justify why not)
