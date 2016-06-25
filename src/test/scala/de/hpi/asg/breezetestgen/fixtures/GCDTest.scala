package de.hpi.asg.breezetestgen.fixtures

import de.hpi.asg.breezetestgen.domain._
import de.hpi.asg.breezetestgen.testing._

import scalax.collection.immutable.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._

object GCDTest {
  val netlist = gcdNetlist()

  def apply(ain: Int, bin: Int, o: Int): Test = {
    def findPort(name: String): Port = netlist.ports.values.find(_.name == name).get
    val activateId = findPort("activate").channelId
    val ainId = findPort("ain").channelId
    val binId = findPort("bin").channelId
    val oId = findPort("o").channelId

    val activateReq = TestEvent.newIOEvent(Request(activateId))
    val activateAck = TestEvent.newIOEvent(Acknowledge(activateId))

    val ainReq = TestEvent.newIOEvent(Request(ainId))
    val ainAck = TestEvent.newIOEvent(DataAcknowledge(ainId, Constant(ain)))

    val binReq = TestEvent.newIOEvent(Request(binId))
    val binAck = TestEvent.newIOEvent(DataAcknowledge(binId, Constant(bin)))

    val oReq = TestEvent.newIOEvent(DataRequest(oId, Constant(o)))
    val oAck = TestEvent.newIOEvent(Acknowledge(oId))

    val merge = TestEvent.newMergeEvent()

    Graph[TestEvent, DiEdge](
      activateReq ~> ainReq,
      activateReq ~> binReq,
      ainReq ~> ainAck,
      binReq ~> binAck,
      ainAck ~> merge,
      binAck ~> merge,
      merge ~> oReq,
      oReq ~> oAck,
      oAck ~> activateAck
    )
  }
}
