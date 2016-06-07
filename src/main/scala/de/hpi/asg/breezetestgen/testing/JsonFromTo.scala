package de.hpi.asg.breezetestgen.testing

import de.hpi.asg.breezetestgen.domain.{Acknowledge, DataAcknowledge, DataRequest, Request}
import net.liftweb.json.CustomSerializer
import net.liftweb.json.JsonAST.{JField, JInt, JObject}

import scalax.collection.io.json._
import scalax.collection.io.json.descriptor.predefined.Di

object JsonFromTo {


  private def idGeneratingDescriptor(): Descriptor[TestEvent] = {
    val idMap = collection.mutable.Map.empty[TestEvent, Int]
    val ids = (1 to Int.MaxValue).iterator

    class MergeEventSerializer extends CustomSerializer[MergeEvent](format => (
      {
        case JObject(JField("id", JInt(id)) :: Nil) =>
          idMap.collectFirst{case (me: MergeEvent, i) if i == id => me}.getOrElse{
            val newMe = new MergeEvent()
            idMap + (newMe -> ids.next())
            newMe
          }
      },
      {
        case x: MergeEvent =>
          val id: Int = idMap.getOrElseUpdate(x, ids.next)
          JObject(JField("id", JInt(id)) :: Nil)
      }
      ))

    val mergeDescriptor = new NodeDescriptor[MergeEvent](
      typeId = "MergeEvent",
      customSerializers = Seq(new MergeEventSerializer)
    ) {
      def id(node: Any) = node match {
        case me: MergeEvent => idMap.getOrElseUpdate(me, ids.next).toString
      }
    }

    val ioEventDescriptor = new NodeDescriptor[IOEvent](typeId = "TestEvent"){
      def id(node: Any) = node match {
        case IOEvent(Request(cId)) => s"Req($cId)"
        case IOEvent(Acknowledge(cId)) => s"Ack($cId)"
        case IOEvent(DataRequest(cId, data)) => s"DataReq($cId, $data)"
        case IOEvent(DataAcknowledge(cId, data)) => s"DataAck($cId, $data)"
      }
    }

    new Descriptor[TestEvent](
      defaultNodeDescriptor = ioEventDescriptor,
      defaultEdgeDescriptor = Di.descriptor[TestEvent](),
      namedNodeDescriptors = Seq(mergeDescriptor),
      namedEdgeDescriptors = Seq(Di.descriptor[TestEvent]())
    )
  }


  def toJson(test: Test): String = {
    test.toJson(idGeneratingDescriptor())
  }

  def fromJson(test: String): Option[Test] = {
    None
  }
}
