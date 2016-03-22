package de.hpi.asg.breezetestgen.domain


object Channel {
  type Id = Int
  type Spec[+Channel] = Id
}

sealed trait Channel[Comp] {
  def id: Channel.Id
  def active: Comp
  def passive: Comp
}

sealed trait NoPullChannel[Comp] extends Channel[Comp]
sealed trait NoPushChannel[Comp] extends Channel[Comp]

final case class SyncChannel[Comp](id: Channel.Id, active: Comp, passive: Comp)
  extends Channel[Comp] with NoPullChannel[Comp] with NoPushChannel[Comp]
final case class PullChannel[Comp](id: Channel.Id, active: Comp, passive: Comp)
  extends Channel[Comp] with NoPushChannel[Comp]
final case class PushChannel[Comp](id: Channel.Id, active: Comp, passive: Comp)
  extends Channel[Comp] with NoPullChannel[Comp]
