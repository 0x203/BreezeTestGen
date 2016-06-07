package de.hpi.asg.breezetestgen.testing

import de.hpi.asg.breezetestgen.domain.{Acknowledge, DataAcknowledge, DataRequest, Request}
import net.liftweb.json.{CustomSerializer, NoTypeHints, Serialization}
import net.liftweb.json.JsonAST.{JField, JInt, JObject}

import scalax.collection.io.json._
import scalax.collection.io.json.descriptor.predefined.Di

object JsonFromTo {
  private class MergeEventSerializer extends CustomSerializer[MergeEvent](format => (
    {
      case JObject(JField("id", JInt(id)) :: Nil) =>
        new MergeEvent() // todo: make use of id.intValue()
    },
    {
      case x: MergeEvent =>
        JObject(JField("id", JInt(2)) :: Nil)
    }
    ))

  private val mergeDescriptor = new NodeDescriptor[MergeEvent](
    typeId = "MergeEvent",
    customSerializers = Seq(new MergeEventSerializer)
  ) {
    def id(node: Any) = "Merge"
  }

  private val ioEventDescriptor = new NodeDescriptor[IOEvent](typeId = "TestEvent"){
    def id(node: Any) = node match {
      case IOEvent(Request(cId)) => s"Req($cId)"
      case IOEvent(Acknowledge(cId)) => s"Ack($cId)"
      case IOEvent(DataRequest(cId, data)) => s"DataReq($cId, $data)"
      case IOEvent(DataAcknowledge(cId, data)) => s"DataAck($cId, $data)"
    }
  }


  def toJson(test: Test): String = {
    val descriptor = new Descriptor[TestEvent](
      defaultNodeDescriptor = ioEventDescriptor,
      defaultEdgeDescriptor = Di.descriptor[TestEvent](),
      namedNodeDescriptors = Seq(mergeDescriptor),
      namedEdgeDescriptors = Seq(Di.descriptor[TestEvent]())
    )

    test.toJson(descriptor)
  }

  def fromJson(test: String): Option[Test] = {
    None
  }
}
