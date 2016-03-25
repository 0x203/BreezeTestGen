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
  val behaviours = netlist.components.mapValues(_.behaviour(None))
  val componentProps = behaviours.mapValues(Props(classOf[ComponentActor], _, infoHub))
  val componentActors = componentProps.mapValues(context.actorOf).view.force

  val portConnectionsIn = portConnectionsOut.map(_.swap)

  def handleSignal(nlId: domain.Netlist.Id, ds: domain.Signal,  te: TestEvent) = {
    if(nlId == netlist.id)
      handleInternal(ds, te)
    else
      handleExternal(ds, te)
  }

  /** lift an internal signal to an external one
    *
    * also, create new TestEvent for this
    */
  private def handleInternal(ds: domain.Signal, te: TestEvent) = {
    val port: Port = netlist.channels.get(ds.channelId).map{ case chan =>
      ds match {
        case _:SignalFromActive => chan.passive
        case _:SignalFromPassive => chan.active
      }
    }.collect{case p:PortEndpoint => p.id}.flatMap(netlist.ports.get).get

    val externalSignal = Signal.changeId(ds, portConnectionsOut(port.id))
    val packedSignal = resolveTestOpAndPack(externalNetlist, te, externalSignal, port)

    pipe(packedSignal) to receiverOf(externalSignal)
  }

  /** adapt an external signal to an internal one */
  private def handleExternal(ds: domain.Signal, te: TestEvent) = {
    val port: Port = portConnectionsIn.get(ds.channelId).flatMap(netlist.ports.get).get
    val channel = netlist.channels(port.channelId)
    val internalReceiverId = (ds match {
      case _:SignalFromActive => channel.passive
      case _:SignalFromPassive => channel.active
    }).asInstanceOf[CompEndpoint].id

    val internalSignal = Signal.changeId(ds, channel.id)
    val packedSignal = resolveTestOpAndPack(netlist.id, te, internalSignal, port)

    pipe(packedSignal) to componentActors(internalReceiverId)
  }

  private def resolveTestOpAndPack(nlId: Netlist.Id,
                                   after: TestEvent,
                                   ds: domain.Signal,
                                   port: Port): Future[HandshakeActor.Signal] = {
    val testOp = ds match {
      case _:Request => AddSyncEvent(after, port.asInstanceOf[SyncPort])
      case _:Acknowledge => AddSyncEvent(after, port.asInstanceOf[SyncPort])
      case DataRequest(_, d) => AddDataEvent(after, port.asInstanceOf[DataPort], d)
      case DataAcknowledge(_, d) => AddDataEvent(after, port.asInstanceOf[DataPort], d)
    }
    val fte: Future[TestEvent] = Future {after}  //TODO: replace this with following line when InfoHub is implemented
    //val fte = (infoHub ? testOp).mapTo[TestEvent]
    fte.map(HandshakeActor.Signal(nlId, ds, _))
  }
}
