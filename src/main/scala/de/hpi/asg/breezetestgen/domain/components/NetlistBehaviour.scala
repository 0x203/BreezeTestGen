package de.hpi.asg.breezetestgen.domain.components

import de.hpi.asg.breezetestgen.domain.{Port, Signal}
import de.hpi.asg.breezetestgen.domain.components.Component.Reaction
import de.hpi.asg.breezetestgen.testing.TestEvent

trait NetlistBehaviour {
  def handleInternalSignal(port: Port, s: Signal, te: TestEvent): Reaction
  def handleExternalSignal(port: Port, s: Signal, te: TestEvent): Reaction
}
