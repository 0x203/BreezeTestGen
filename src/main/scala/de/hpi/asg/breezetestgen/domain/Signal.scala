package de.hpi.asg.breezetestgen.domain

object Signal {
  def changeChannelId(s: Signal, newChannelId: Channel.Id) = s match {
    case r: Request => Request(newChannelId)
    case a: Acknowledge => Acknowledge(newChannelId)
    case dr: DataRequest => dr.copy(channelId = newChannelId)
    case da: DataAcknowledge => da.copy(channelId = newChannelId)
  }

  val ActivateAcknowledge = Acknowledge(1)
}

sealed trait Signal {
  def channelId: Channel.Spec[Channel[Channel.Endpoint]]
}

sealed trait SignalFromActive extends Signal
sealed trait SignalFromPassive extends Signal

sealed trait SignalWithData extends Signal {
  val data: Data
}

case class Request(channelId: Channel.Spec[NoPushChannel[Channel.Endpoint]]) extends SignalFromActive
case class Acknowledge(channelId: Channel.Spec[NoPullChannel[Channel.Endpoint]]) extends SignalFromPassive
case class DataRequest(channelId: Channel.Spec[PushChannel[Channel.Endpoint]], data: Data)
  extends SignalFromActive with SignalWithData
case class DataAcknowledge(channelId: Channel.Spec[PullChannel[Channel.Endpoint]], data: Data)
  extends SignalFromPassive with SignalWithData
