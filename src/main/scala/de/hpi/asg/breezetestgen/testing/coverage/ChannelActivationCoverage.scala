package de.hpi.asg.breezetestgen.testing.coverage

import de.hpi.asg.breezetestgen.domain.{Channel, Netlist, Signal, SignalFromPassive}

object ChannelActivationCoverage {
  def forNetlist(netlist: Netlist): ChannelActivationCoverage =
    ChannelActivationCoverage(netlist, Set.empty)
}

case class ChannelActivationCoverage(netlist: Netlist, coveredChannels: Set[Channel.Id]) extends Coverage {
  private def allChannels = netlist.channels.keySet

  def withSignal(signal: Signal): Coverage =
    if (isCovered(signal) || signal.isInstanceOf[SignalFromPassive]) {
      this
    } else {
      this.copy(coveredChannels = coveredChannels + signal.channelId)
    }

  def withSignals(signals: Traversable[Signal]): Coverage =
    this.copy(coveredChannels = coveredChannels ++ signals.map(_.channelId))

  def isComplete: Boolean = (allChannels.size - coveredChannels.size) == 0
  def percentageCovered: Double = coveredChannels.size.toDouble * 100 / allChannels.size

  def isCovered(signal: Signal): Boolean = coveredChannels contains signal.channelId

  def merge(other: Coverage): Coverage =
    other match {
      case ChannelActivationCoverage(`netlist`, otherCoveredChannels) =>
        this.copy(coveredChannels = coveredChannels ++ otherCoveredChannels)
      case _: ChannelActivationCoverage =>
        throw new IllegalArgumentException("Merging just works if this and other share the same netlist")
      case _ =>
        throw new NotImplementedError()
    }
}
