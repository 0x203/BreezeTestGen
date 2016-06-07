package de.hpi.asg.breezetestgen.testing

import de.hpi.asg.breezetestgen.domain.{Acknowledge, DataAcknowledge, DataRequest, Request}
import net.liftweb.json.CustomSerializer
import net.liftweb.json.JsonAST.{JField, JInt, JObject}

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef
import scalax.collection.io.json._
import scalax.collection.io.json.descriptor.predefined.Di

/** Helps transforming an instance of a test graph to a json representation and the other way round
  */
object JsonFromTo {

  /** adds an ID to an IOEvent in order to be adresseable by edges */
  private case class TestEventWithID(id: Int, orig: TestEvent)

  /** transform a testgrpah without IDs to one with IDs */
  private def addIDs(orig: Test): scalax.collection.Graph[TestEventWithID, DiEdge] = {
    val ids = (1 to Int.MaxValue).iterator

    val nodeMap = orig.nodes
      .map{ x => x -> TestEventWithID(ids.next(), x)}
      .toMap

    // replace placeholders in all edge definitions
    val edges: Set[GraphPredef.Param[TestEventWithID, DiEdge]] = Set() ++ orig.edges.map{
      e => DiEdge(nodeMap(e.source), nodeMap(e.target)).asInstanceOf[GraphPredef.Param[TestEventWithID, DiEdge]]
    }

    // create a new, immutable graph from the edges and nodes
    scalax.collection.Graph[TestEventWithID, DiEdge]() ++ (edges ++ nodeMap.values.map(GraphPredef.OuterNode[TestEventWithID]))
  }


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
    val testWithIds = addIDs(test)

    val nodeDescriptor = new NodeDescriptor[TestEventWithID](typeId = "TestEvent"){
      def id(node: Any) = node match {
        case TestEventWithID(id, _) => id.toString
      }
    }
    val desc = new Descriptor[TestEventWithID](
      defaultNodeDescriptor = nodeDescriptor,
      defaultEdgeDescriptor = Di.descriptor[TestEventWithID](),
      namedNodeDescriptors = Seq(nodeDescriptor),
      namedEdgeDescriptors = Seq(Di.descriptor[TestEventWithID]())
    )

    testWithIds.toJson(desc)
  }

  def fromJson(test: String): Option[Test] = {
    None
  }
}
