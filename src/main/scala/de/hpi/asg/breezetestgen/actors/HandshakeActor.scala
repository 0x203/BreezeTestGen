package de.hpi.asg.breezetestgen.actors

import akka.actor.ActorRef
import de.hpi.asg.breezetestgen.domain
import de.hpi.asg.breezetestgen.testing.TestEvent
import domain.Channel

object HandshakeActor {
  type ChannelMap = Channel.Id => Channel[ActorRef]
  case class SetChannels(channels: ChannelMap)

  case class Signal[DT <: domain.Data](domainSignal: domain.Signal, testEvent: TestEvent)
}
