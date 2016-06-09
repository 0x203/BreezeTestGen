package de.hpi.asg.breezetestgen

import akka.actor.Props
import de.hpi.asg.breezetestgen.actors.HandshakeActor.{GetState, MyState, SetChannels}
import de.hpi.asg.breezetestgen.actors.{ComponentActor, HandshakeActor}
import de.hpi.asg.breezetestgen.baseclasses.AkkaIntegrationSpec
import de.hpi.asg.breezetestgen.domain._
import de.hpi.asg.breezetestgen.domain.components.{BrzComponentBehaviour, HandshakeComponent}
import de.hpi.asg.breezetestgen.domain.components.brzcomponents.Fetch
import de.hpi.asg.breezetestgen.testing.IOEvent

class ComponentActorSpec extends AkkaIntegrationSpec("ComponentActorSpec") {
  val nlId = 0 :: Nil
  val compId = 1
  val (aChan, iChan, oChan) = (5, 6, 7)
  val sampleFetch = new Fetch(compId, aChan, iChan, oChan)
  val sampleTestEvent = IOEvent(Request(700))

  def freshBehaviour() = sampleFetch.behaviour(None)

  val (stateAfterReq, stateAfterReqAndData) = {
    val behav = freshBehaviour()
    behav.handleSignal(Request(aChan), sampleTestEvent)
    val tmp = behav.state
    behav.handleSignal(DataAcknowledge(iChan, Constant(42)), sampleTestEvent)
    (tmp, behav.state)
  }

  def newActor(behaviour: BrzComponentBehaviour[_, _]) =
    system.actorOf(Props(classOf[ComponentActor], nlId, compId, behaviour, None))

  val setChannels = SetChannels(id => SyncChannel(id, self, self)) // type of channel wont get checked

  def toHSASignal(signal: Signal): HandshakeActor.Signal = HandshakeActor.Signal(nlId, signal, sampleTestEvent)
  def toMyState(state: HandshakeComponent.State[_, _]): MyState = MyState(nlId, compId, state)

  "A ComponentActor" should "react correctly according to behaviour" in {
    val uut = newActor(freshBehaviour())

    uut ! setChannels

    uut ! toHSASignal(Request(aChan))
    expectMsg(toHSASignal(Request(iChan)))
  }

  it should "answer with current state if asked" in {
    val uninit = newActor(freshBehaviour())
    uninit ! GetState
    expectMsg(toMyState(sampleFetch.FetchBehaviour.freshState))

    val init = newActor(freshBehaviour())
    init ! setChannels
    init ! GetState
    expectMsg(toMyState(sampleFetch.FetchBehaviour.freshState))

    val used = newActor(freshBehaviour())
    used ! setChannels
    used ! toHSASignal(Request(aChan))
    expectMsgClass(classOf[HandshakeActor.Signal])
    used ! GetState
    expectMsg(toMyState(stateAfterReq))
  }

  it should "be initialized in given state" in {
    val uut = newActor(sampleFetch.behaviour(Option(stateAfterReq)))
    uut ! setChannels

    val data = Constant(232)
    uut ! toHSASignal(DataAcknowledge(iChan, data))
    expectMsg(toHSASignal(DataRequest(oChan, data)))

    uut ! GetState
    expectMsg(toMyState(stateAfterReqAndData))
  }
}
