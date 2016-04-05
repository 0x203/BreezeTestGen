package de.hpi.asg.breezetestgen.fixtures

import de.hpi.asg.breezetestgen.domain._
import de.hpi.asg.breezetestgen.testing._

import scalax.collection.immutable.Graph
import scalax.collection.GraphEdge.DiEdge

object GCDTest {
  val netlist = gcdNetlist()

  def apply(ain: Int, bin: Int, o: Int): Test = {
    def findPort(name: String): Port = netlist.ports.values.find(_.name == name).get
    val activateId = findPort("activate").channelId
    val ainId = findPort("ain").channelId
    val binId = findPort("bin").channelId
    val oId = findPort("o").channelId

    val activateReq = IOEvent(Request(activateId))
    val activateAck = IOEvent(Acknowledge(activateId))

    val ainReq = IOEvent(Request(ainId))
    val ainAck = IOEvent(DataAcknowledge(ainId, Constant(ain)))

    val binReq = IOEvent(Request(binId))
    val binAck = IOEvent(DataAcknowledge(binId, Constant(bin)))

    val oReq = IOEvent(DataRequest(oId, Constant(o)))
    val oAck = IOEvent(Acknowledge(oId))

    val merge = new MergeEvent

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
