package de.hpi.asg.breezetestgen.testing

import de.hpi.asg.breezetestgen.domain.Signal

sealed trait TestEvent {
  val id: TestEvent.Id
}

case class MergeEvent(id: TestEvent.Id) extends TestEvent
case class IOEvent(id: TestEvent.Id, signal: Signal) extends TestEvent

object TestEvent {
  type Id = Int
  private var currentId = 1
  private def nextId(): TestEvent.Id = {
    val r = currentId
    currentId += 1
    r
  }

  def newMergeEvent(): MergeEvent = MergeEvent(nextId())
  def newIOEvent(signal: Signal): IOEvent = IOEvent(nextId(), signal)
}
