package de.hpi.asg.breezetestgen.testing

import de.hpi.asg.breezetestgen.domain._
import net.liftweb.json.{CustomSerializer, DefaultFormats}
import net.liftweb.json.JsonAST.{JField, JInt, JObject, JString}
import net.liftweb.json.Extraction.decompose

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef
import scalax.collection.io.json._
import scalax.collection.io.json.descriptor.predefined.Di


/** Helps transforming an instance of a test graph to a json representation and the other way round
  */
object JsonFromTo {
  def toJson(test: Test): String = {
    val testWithIds = addIDs(test)

    testWithIds.toJson(desc)
  }

  def fromJson(test: String): Option[Test] = {
    None
  }

  private implicit val formats = DefaultFormats

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

  private class TestEventWithIDSerializer extends CustomSerializer[TestEventWithID](format => (
    {
      // TODO: implement extraction of events
      case JObject(JField("id", JInt(id)) :: Nil) =>
        TestEventWithID(1, new MergeEvent)
    },
    {
      case TestEventWithID(id, _:MergeEvent) =>
        JObject(JField("type", JString("Merge")) :: JField("id", JInt(id)) :: Nil)

      case TestEventWithID(id, IOEvent(s: Signal)) =>
        val d = decompose(s)
        val typeString = s match {
          case _: Request => "Request"
          case _: Acknowledge => "Acknowledge"
          case _: DataRequest => "DataRequest"
          case _: DataAcknowledge => "DataAcknowledge"
        }
        JObject(JField("type", JString(typeString)) :: JField("id", JInt(id)) :: Nil) merge d
    }
    ))

  private val nodeDescriptor = new NodeDescriptor[TestEventWithID](
    typeId = "TestEvent",
    customSerializers = Seq(new TestEventWithIDSerializer)
  ){
    def id(node: Any) = node match {
      case TestEventWithID(id, _) => id.toString
    }
  }
  private val desc = new Descriptor[TestEventWithID](
    defaultNodeDescriptor = nodeDescriptor,
    defaultEdgeDescriptor = Di.descriptor[TestEventWithID](),
    namedNodeDescriptors = Seq(nodeDescriptor),
    namedEdgeDescriptors = Seq(Di.descriptor[TestEventWithID]())
  )
}
