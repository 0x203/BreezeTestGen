package de.hpi.asg.breezetestgen.domain

import de.hpi.asg.breezetestgen.domain.components.HandshakeComponent


object Channel {
  type Id = Int
  type Spec[+Channel] = Id

  sealed trait Endpoint
  case class CompEndpoint(id: HandshakeComponent.Id) extends Endpoint
  case class PortEndpoint(id: Port.Id) extends Endpoint

  // the next one exists because there was a strange exception when i tried creating it directly
  def newCompEndpoint(netlistId: Netlist.Id, i: Int): CompEndpoint = CompEndpoint(netlistId :+ i)

  implicit def brzCompId2CompEndpoint(id: HandshakeComponent.Id): CompEndpoint = CompEndpoint(id)
  implicit def portId2PortEndpoint(id: Port.Id): PortEndpoint = PortEndpoint(id)
}

sealed trait Channel[Comp] {
  def id: Channel.Id
  def active: Comp
  def passive: Comp

  def transform[NewComp](f: Comp => NewComp): Channel[NewComp]
}

sealed trait NoPullChannel[Comp] extends Channel[Comp]
sealed trait NoPushChannel[Comp] extends Channel[Comp]

final case class SyncChannel[Comp](id: Channel.Id, active: Comp, passive: Comp)
  extends Channel[Comp] with NoPullChannel[Comp] with NoPushChannel[Comp] {
  def transform[NewComp](f: Comp => NewComp): SyncChannel[NewComp] = this.copy(active=f(active), passive=f(passive))
}

final case class PullChannel[Comp](id: Channel.Id, active: Comp, passive: Comp)
  extends Channel[Comp] with NoPushChannel[Comp] {
  def transform[NewComp](f: Comp => NewComp): PullChannel[NewComp] = this.copy(active=f(active), passive=f(passive))
}

final case class PushChannel[Comp](id: Channel.Id, active: Comp, passive: Comp)
  extends Channel[Comp] with NoPullChannel[Comp] {
  def transform[NewComp](f: Comp => NewComp): PushChannel[NewComp] = this.copy(active=f(active), passive=f(passive))
}
