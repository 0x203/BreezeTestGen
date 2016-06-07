package de.hpi.asg.breezetestgen.testing

import de.hpi.asg.breezetestgen.domain.Signal

sealed trait TestEvent

class MergeEvent() extends TestEvent

case class IOEvent(signal: Signal) extends TestEvent
