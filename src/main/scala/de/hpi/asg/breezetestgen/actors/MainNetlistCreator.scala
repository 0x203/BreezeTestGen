package de.hpi.asg.breezetestgen.actors

import akka.actor.{Actor, ActorRef, Props}
import de.hpi.asg.breezetestgen.domain.{Channel, Netlist, Port, SyncChannel}

trait MainNetlistCreator {
  this: Actor =>

  protected def netlist: Netlist

  // hacky way to create channelMaps with all ports of MainNetlist pointing to this simulator-actor
  private val netlistChannelMap = HandshakeActor.SetChannels(
    netlist.ports.map{case (_, port) => port.channelId -> SyncChannel(port.channelId, self, self)}
  )

  protected val portConnections = netlist.ports.values.map{p => p.id -> p.channelId}.toMap[Port.Id, Channel.Id]

  /** creates a new netlist actor for the netlist to be simulated */
  protected def newNetlistActor(externalId: Netlist.Id,
                                state: Option[Netlist.State] = None,
                                infoHub: Option[ActorRef] = None): ActorRef = {
    val props = Props(classOf[NetlistActor], netlist, externalId :: Nil, portConnections, state, infoHub)

    val newActor = context.actorOf(props, s"Test$externalId-MainNetlist")
    newActor ! netlistChannelMap
    newActor
  }

}
