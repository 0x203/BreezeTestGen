package de.hpi.asg.breezetestgen.actors

import akka.actor.{ActorRef, Props, Terminated}
import de.hpi.asg.breezetestgen.Loggable
import de.hpi.asg.breezetestgen.actors.HandshakeActor.{Decision, GetState, MyState}
import de.hpi.asg.breezetestgen.domain
import de.hpi.asg.breezetestgen.domain.Channel.{CompEndpoint, PortEndpoint}
import de.hpi.asg.breezetestgen.domain._
import de.hpi.asg.breezetestgen.domain.components.{BrzComponent, BrzComponentBehaviour, HandshakeComponent}
import de.hpi.asg.breezetestgen.testing.TestEvent


class NetlistActor(runId: Int,
                   netlist: domain.Netlist,
                   portConnectionsOut: Map[Port.Id, Channel.Id],
                   initialState: Option[Netlist.State],
                   infoHub: Option[ActorRef]) extends HandshakeActor with Loggable {
  info(s"NetlistActor created for netlist id ${netlist.id}")

  private val idOfParent = netlist.id.take(netlist.id.size - 1)
  private val componentActors = netlist.components
    .mapValues(createBehaviour)
    .map{case (id, comp) => id -> Props(classOf[ComponentActor], runId, id, comp, infoHub)}  //create Props
    .mapValues(context.actorOf)
    .view.force

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
  componentActors.values.foreach(context.watch)

  def handleSignal(senderId: HandshakeComponent.Id, ds: domain.Signal,  te: TestEvent) = {
    info(s"getting: $ds")
    val (outgoingId, newSignal, receiver) =
      if (senderId == idOfParent) handleExternal(ds) else handleInternal(ds)

    val packedSignal = HandshakeActor.Signal(runId, outgoingId, newSignal, te)

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
  private type HandlerResult = (HandshakeComponent.Id, domain.Signal, ActorRef)

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

    (idOfParent, externalSignal, receiver)
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

  // state gathering
  private def freshComponentStates() =
    collection.mutable.Map.empty[HandshakeComponent.Id, HandshakeComponent.State[_, _]]
  var componentStates = freshComponentStates()
  var stateInvoker: ActorRef = _

  receiue{
    case GetState =>
      info("Start gathering component states")
      stateInvoker = sender()
      componentActors.values.foreach{_ ! GetState}

    case MyState(`runId`, compId, compState) =>
      componentStates += compId -> compState

      if(componentStates.size == netlist.components.size) {
        info("Got all component states")
        stateInvoker ! MyState(
            runId,
            netlist.id,
            Netlist.State(collection.immutable.Map.empty ++ componentStates)
        )
        componentStates = freshComponentStates()
      }

    case Decision(componentId, ns, ds, te) =>
      info("Got Decision attempting to forward")
      componentId.drop(netlist.id.size) match {
        case cId :: Nil => componentActors(componentId) ! Decision(componentId, ns, ds, te)
        case subNetlistId :: _ =>
          // TODO: implement me for hierarchical netlists
          // subNetlists(netlist.id :+ subNetlistId) ! Decision(componentId, ns, ds, te)
          throw new NotImplementedError()
      }

    case Terminated(_) =>
      error("A component actor of mine died. I'll follow him.")
      componentActors.values.foreach(context.stop)
      context.stop(self)
  }
}
