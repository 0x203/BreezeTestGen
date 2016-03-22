package de.hpi.asg.breezetestgen.domain

sealed trait Signal[+D <: Data] {
  def channelId: Channel.Id
}
sealed trait SignalFromActive[+D <: Data] extends Signal[D]
sealed trait SignalFromPassive[+D <: Data] extends Signal[D]
case class Request(channelId: Channel.Spec[NoPushChannel[_]]) extends SignalFromActive[Nothing]
case class Acknowledge(channelId: Channel.Spec[NoPullChannel[_]]) extends SignalFromPassive[Nothing]
case class DataRequest[+D <: Data](channelId: Channel.Spec[PushChannel[_]], data: D) extends SignalFromActive[D]
case class DataAcknowledge[+D <: Data](channelId: Channel.Spec[PullChannel[_]], data: D) extends SignalFromPassive[D]
