package de.hpi.asg.breezetestgen.testing.coverage

import de.hpi.asg.breezetestgen.domain.{Netlist, Signal}

trait Coverage {
  val netlist: Netlist

  /** return a new coverage instance with this signal already covered */
  def withSignal(signal: Signal): Coverage

  def isComplete: Boolean
  def percentageCovered: Double

  /** returns, if adding this signal would make any difference in the covered percentage */
  def isCovered(signal: Signal): Boolean
}
