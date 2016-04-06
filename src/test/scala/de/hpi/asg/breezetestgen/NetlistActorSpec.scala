package de.hpi.asg.breezetestgen

import concurrent.duration._
import akka.actor.{ActorRef, Props}
import de.hpi.asg.breezetestgen.actors.HandshakeActor.{GetState, MyState, SetChannels}
import de.hpi.asg.breezetestgen.actors.{HandshakeActor, NetlistActor}
import de.hpi.asg.breezetestgen.baseclasses.AkkaIntegrationSpec
import de.hpi.asg.breezetestgen.domain._
import fixtures.gcdNetlist

class NetlistActorSpec extends AkkaIntegrationSpec("NetlistActorSpec") {
  val netlist = gcdNetlist()
  val extId = -1
  val portConnections = netlist.ports.values.map{p => p.id -> p.channelId}.toMap[Port.Id, Channel.Id]

  val freshState = Netlist.State(netlist.components.mapValues(_.behaviour(None).state))
  val setChannels = SetChannels(id => SyncChannel(id, self, self)) // type of channel wont get checked

  def newActor(state: Option[Netlist.State]) = system.actorOf(
    Props(classOf[NetlistActor], netlist, extId, portConnections, state, None)
  )

  def assertState(uut: ActorRef, state: Netlist.State) = {
    uut ! GetState
    expectMsg(MyState(extId, netlist.id, state))
  }

  "A NetlistActor" should "create all components in fresh state" in {
    assertState(newActor(None), freshState)
  }

  it should "be resumeable from a restored state" in {
    val actor1 = newActor(None)
    actor1 ! setChannels

    actor1 ! HandshakeActor.Signal(extId, Request(1), null)
    expectMsgClass(classOf[HandshakeActor.Signal])  // ain (or bin)
    expectMsgClass(classOf[HandshakeActor.Signal])  // bin (or ain)

    actor1 ! GetState
    val maybeState = receiveOne(1 seconds)
    assert(maybeState.isInstanceOf[MyState])
    val myState = maybeState.asInstanceOf[MyState]
    assert(myState.netlistId == extId)
    assert(myState.id == netlist.id)

    system.stop(actor1)

    val preservedStateTmp = myState.state
    assert(preservedStateTmp.isInstanceOf[Netlist.State])
    val preservedState = preservedStateTmp.asInstanceOf[Netlist.State]

    val actor2 = newActor(Option(preservedState))
    actor2 ! setChannels

    assertState(actor2, preservedState)

    actor2 ! HandshakeActor.Signal(extId, DataAcknowledge(2, Constant(15)), null)
    actor2 ! HandshakeActor.Signal(extId, DataAcknowledge(3, Constant(7)), null)
    expectMsg(HandshakeActor.Signal(extId, DataRequest(4, Constant(1)), null))
  }
}
