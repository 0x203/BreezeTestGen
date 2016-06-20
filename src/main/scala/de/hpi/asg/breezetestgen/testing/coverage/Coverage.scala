package de.hpi.asg.breezetestgen.testing.coverage

import de.hpi.asg.breezetestgen.domain.{Netlist, Signal}

trait Coverage {
  val netlist: Netlist

  /** return a new coverage instance with this signal covered additionally */
  def withSignal(signal: Signal): Coverage

  /** return a new coverage instance with this signals covered additionally */
  def withSignals(signals: Traversable[Signal]): Coverage

  def isComplete: Boolean
  def percentageCovered: Double

  /** returns, if adding this signal would make any difference in the covered percentage */
  def isCovered(signal: Signal): Boolean

  /** new coverage instance covering everything this and other cover */
  def merge(other: Coverage): Coverage
}
