package de.hpi.asg.breezetestgen.domain.components

import de.hpi.asg.breezetestgen.domain.Signal
import de.hpi.asg.breezetestgen.domain.components.Component.Reaction
import de.hpi.asg.breezetestgen.testing.TestEvent

trait NetlistBehaviour {
  def handleInternalSignal(s: Signal, te: TestEvent): Reaction
  def handleExternalSignal(s: Signal, te: TestEvent): Reaction
}
