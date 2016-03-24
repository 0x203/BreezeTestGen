package de.hpi.asg.breezetestgen.domain

sealed trait Signal {
  def channelId: Channel.Spec[Channel[Channel.Endpoint]]
}

sealed trait SignalFromActive extends Signal
sealed trait SignalFromPassive extends Signal

case class Request(channelId: Channel.Spec[NoPushChannel[Channel.Endpoint]]) extends SignalFromActive
case class Acknowledge(channelId: Channel.Spec[NoPullChannel[Channel.Endpoint]]) extends SignalFromPassive
case class DataRequest(channelId: Channel.Spec[PushChannel[Channel.Endpoint]], data: Data) extends SignalFromActive
case class DataAcknowledge(channelId: Channel.Spec[PullChannel[Channel.Endpoint]], data: Data) extends SignalFromPassive
