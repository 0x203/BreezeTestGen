package de.hpi.asg.breezetestgen.actors

import akka.actor.{ActorRef, Props}
import akka.pattern.{ask, pipe}
import de.hpi.asg.breezetestgen.Loggable
import de.hpi.asg.breezetestgen.domain
import de.hpi.asg.breezetestgen.domain.Channel.{CompEndpoint, PortEndpoint}
import de.hpi.asg.breezetestgen.domain._
import de.hpi.asg.breezetestgen.testgeneration.{AddDataEvent, AddSyncEvent}
import de.hpi.asg.breezetestgen.testing.TestEvent

import scala.concurrent.Future


class NetlistActor(netlist: domain.Netlist,
                   externalNetlist: Netlist.Id,
                   portConnectionsOut: Map[Port.Id, Channel.Id],
                   infoHub: ActorRef) extends HandshakeActor with Loggable {
  import context.dispatcher   // as ExecutionContext for futures

  info(s"NetlistActor created for netlist id ${netlist.id}")

  //TODO: insert state here
  val behaviours = netlist.components.mapValues(_.behaviour(None))
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
  override def preStart() {
    self ! setChannels
    //self ! HandshakeActor.Signal(1, Request(0), null)
  }

  def handleSignal(nlId: domain.Netlist.Id, ds: domain.Signal,  te: TestEvent) = {
    info(s"getting: $ds")
    val (netlistId, newSignal, port, receiver) =
      if (nlId == netlist.id) handleInternal(ds, te) else handleExternal(ds, te)

    val testOp = ds match {
      case _:Request => AddSyncEvent(te, port.asInstanceOf[SyncPort])
      case _:Acknowledge => AddSyncEvent(te, port.asInstanceOf[SyncPort])
      case DataRequest(_, d) => AddDataEvent(te, port.asInstanceOf[DataPort], d)
      case DataAcknowledge(_, d) => AddDataEvent(te, port.asInstanceOf[DataPort], d)
    }
    val fte: Future[TestEvent] = Future {te}  //TODO: replace this with following line when InfoHub is implemented
    //val fte = (infoHub ? testOp).mapTo[TestEvent]

    val packedSignal = fte.map(HandshakeActor.Signal(netlistId, newSignal, _))

    pipe(packedSignal) to receiver
  }

  /** return type of internal and external handler */
  private type HandlerResult = (Netlist.Id, domain.Signal, Port, ActorRef)

  /** lift an internal signal to an external one
    *
    * also, create new TestEvent for this
    */
  private def handleInternal(ds: domain.Signal, te: TestEvent): HandlerResult = {
    val port: Port = netlist.channels.get(ds.channelId).map{ case chan =>
      ds match {
        case _:SignalFromActive => chan.passive
        case _:SignalFromPassive => chan.active
      }
    }.collect{case p:PortEndpoint => p.id}.flatMap(netlist.ports.get).get

    val externalSignal = Signal.changeId(ds, portConnectionsOut(port.id))

    (externalNetlist, externalSignal, port, receiverOf(externalSignal))
  }

  /** adapt an external signal to an internal one */
  private def handleExternal(ds: domain.Signal, te: TestEvent): HandlerResult = {
    val port: Port = portConnectionsIn.get(ds.channelId).flatMap(netlist.ports.get).get
    val channel = netlist.channels(port.channelId)
    val internalReceiverId = (ds match {
      case _:SignalFromActive => channel.passive
      case _:SignalFromPassive => channel.active
    }).asInstanceOf[CompEndpoint].id

    val internalSignal = Signal.changeId(ds, channel.id)

    (netlist.id, internalSignal, port, componentActors(internalReceiverId))
  }
}
