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

  def tryCompareTo[B >: Coverage](that: B)(implicit evidence: B => PartiallyOrdered[B]): Option[Int] =
    that match {
      case other: ChannelActivationCoverage =>
        val unionSize = coveredChannels.union(other.coveredChannels).size
        if(coveredChannels == other.coveredChannels)      // both cover the same
          Some(0)
        else if (other.coveredChannels.size == unionSize) // I don't add anything to other
          Some(-1)
        else if (coveredChannels.size == unionSize)       // other doesn't add anything to me
          Some(1)
        else None                                         // we cover different parts of the netlist
      case _ => None
    }
}
