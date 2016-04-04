package de.hpi.asg.breezetestgen.testing

import de.hpi.asg.breezetestgen.domain.{Constant, Signal}
import scalax.collection.GraphEdge.DiEdge


sealed trait TestEvent {
  //TODO: remove this when next version of graph library is released
  def ~>(other: TestEvent): DiEdge[TestEvent] = DiEdge(this, other)
}

class MergeEvent() extends TestEvent

case class IOEvent(signal: Signal) extends TestEvent
