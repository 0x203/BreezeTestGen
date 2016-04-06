package de.hpi.asg.breezetestgen.actors

import akka.actor.{ActorRef, Props}
import de.hpi.asg.breezetestgen.Loggable
import de.hpi.asg.breezetestgen.domain
import de.hpi.asg.breezetestgen.domain.Channel.{CompEndpoint, PortEndpoint}
import de.hpi.asg.breezetestgen.domain._
import de.hpi.asg.breezetestgen.domain.components.{BrzComponent, BrzComponentBehaviour, HandshakeComponent}
import de.hpi.asg.breezetestgen.testing.TestEvent


class NetlistActor(netlist: domain.Netlist,
                   externalNetlist: Netlist.Id,
                   portConnectionsOut: Map[Port.Id, Channel.Id],
                   initialState: Option[Netlist.State],
                   infoHub: ActorRef) extends HandshakeActor with Loggable {
  info(s"NetlistActor created for netlist id ${netlist.id}")

  val behaviours = netlist.components.mapValues(createBehaviour)
  val componentProps = behaviours.mapValues(Props(classOf[ComponentActor], _, infoHub))
  val componentActors = componentProps.mapValues(context.actorOf).view.force

  val portConnectionsIn = portConnectionsOut.map(_.swap)

  // maps channelEndpoint to corresponding Actors
  // - for CompEndpoints this is the respective component
  // - for ports it is the netlist itself
  private def componentEndpointToActorRef(ep: Channel.Endpoint) = ep match {
    case ce: CompEndpoint => componentActors(ce.id)
    case pe: PortEndpoint => self
  }
  // creates a channelMap to initialize the components
  val channelMap: Map[Channel.Id, Channel[ActorRef]] = netlist.channels.map{case (id, chan) =>
      id -> chan.transform(componentEndpointToActorRef)
  }
  val setChannels = HandshakeActor.SetChannels(channelMap)
  componentActors.values.foreach(_ ! setChannels)

  def handleSignal(nlId: domain.Netlist.Id, ds: domain.Signal,  te: TestEvent) = {
    info(s"getting: $ds")
    val (netlistId, newSignal, receiver) =
      if (nlId == netlist.id) handleInternal(ds) else handleExternal(ds)

    val packedSignal = HandshakeActor.Signal(netlistId, newSignal, te)

    receiver ! packedSignal
  }

  /** create [[BrzComponentBehaviour]] with state from initial state */
  private def createBehaviour(component: BrzComponent): BrzComponentBehaviour[_, _] = {
    val state = initialState
      .flatMap(_.dataState.get(component.id)) // get the state of the current component
      .map(_.asInstanceOf[HandshakeComponent.State[component.C, component.D]])  // cast it to the right type

    component.behaviour(state)
  }

  /** return type of internal and external handler */
  private type HandlerResult = (Netlist.Id, domain.Signal, ActorRef)

  /** lift an internal signal to an external one */
  private def handleInternal(ds: domain.Signal): HandlerResult = {
    def otherSideOfChannel[X](chan: Channel[X]): X = ds match {
      case _:SignalFromActive => chan.passive
      case _:SignalFromPassive => chan.active
    }

    val externalChannel: Channel.Id = netlist.channels
      .get(ds.channelId)         // get channel
      .map(otherSideOfChannel)   // get PortEndpoint
      .collect{case p:PortEndpoint => p.id} // map it to it's id
      .flatMap(portConnectionsOut.get)  // map it to external channel
      .get  // unpack it

    val externalSignal = Signal.changeChannelId(ds, externalChannel)
    val receiver = channels.andThen(otherSideOfChannel)(externalChannel)

    (externalNetlist, externalSignal, receiver)
  }

  /** adapt an external signal to an internal one */
  private def handleExternal(ds: domain.Signal): HandlerResult = {
    val port: Port = portConnectionsIn.get(ds.channelId).flatMap(netlist.ports.get).get
    val channel = netlist.channels(port.channelId)
    val internalReceiverId = (ds match {
      case _:SignalFromActive => channel.passive
      case _:SignalFromPassive => channel.active
    }).asInstanceOf[CompEndpoint].id

    val internalSignal = Signal.changeChannelId(ds, channel.id)

    (netlist.id, internalSignal, componentActors(internalReceiverId))
  }
}
