package de.hpi.asg.breezetestgen.domain

sealed trait Signal {
  def channelId: Channel.Id
}

sealed trait SignalFromActive extends Signal
sealed trait SignalFromPassive extends Signal

case class Request(channelId: Channel.Spec[NoPushChannel[_]]) extends SignalFromActive
case class Acknowledge(channelId: Channel.Spec[NoPullChannel[_]]) extends SignalFromPassive
case class DataRequest(channelId: Channel.Spec[PushChannel[_]], data: Data) extends SignalFromActive
case class DataAcknowledge(channelId: Channel.Spec[PullChannel[_]], data: Data) extends SignalFromPassive
