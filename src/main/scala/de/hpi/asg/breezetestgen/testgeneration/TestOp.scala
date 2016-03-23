package de.hpi.asg.breezetestgen.testgeneration

import de.hpi.asg.breezetestgen.domain.{Data, DataPort, SyncPort}
import de.hpi.asg.breezetestgen.testing.TestEvent


sealed trait TestOp
case class Merge(after: Set[TestEvent]) extends TestOp
case class AddSyncEvent(after: TestEvent, syncPort: SyncPort) extends TestOp
case class AddDataEvent(after: TestEvent, dataPort: DataPort, d: Data) extends TestOp
