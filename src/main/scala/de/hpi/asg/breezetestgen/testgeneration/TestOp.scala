package de.hpi.asg.breezetestgen.testgeneration

import de.hpi.asg.breezetestgen.testing.TestEvent

sealed trait TestOp
case class Follow(after: TestEvent) extends TestOp
case class Merge(after: Set[TestEvent]) extends TestOp
