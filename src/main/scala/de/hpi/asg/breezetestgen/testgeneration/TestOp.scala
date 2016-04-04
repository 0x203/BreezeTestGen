package de.hpi.asg.breezetestgen.testgeneration

import de.hpi.asg.breezetestgen.domain.Signal
import de.hpi.asg.breezetestgen.testing.TestEvent


sealed trait TestOp
case class Merge(after: Set[TestEvent]) extends TestOp
case class AddIOEvent(after: TestEvent, signal: Signal) extends TestOp
