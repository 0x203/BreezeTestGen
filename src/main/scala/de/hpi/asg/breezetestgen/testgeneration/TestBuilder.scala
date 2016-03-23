package de.hpi.asg.breezetestgen.testgeneration

import de.hpi.asg.breezetestgen.constraintsolving.Variable
import de.hpi.asg.breezetestgen.domain.{SyncPort, Port, DataPort}
import de.hpi.asg.breezetestgen.testing._

import scalax.collection._
import scalax.collection.GraphEdge._


object TestBuilder {
  /** a function used to fixate a variable to a concrete value */
  type VariableFixator = Variable => Int


  /** Used as unfixed-value placeholder for [[IODataEvent]]s during gathering of IOEvents
    *
    * A [[TestBuilder]] gathers IOEvents without knowledge of concrete values for the according events.
    * Objects of this class are used as a placeholder for IODataEvents, which can be assigned to real objects
    * of this type once the variables are fixed.
    *
    * @param port will become the port of the IODataEvent
    * @param valueVariable placeholder for fixed value
    */
  private case class IODataEventPlaceholder(port: DataPort, valueVariable: Variable) extends IOEvent[DataPort] {
    def toIODataEvent(f: VariableFixator): IODataEvent = IODataEvent(port, f(valueVariable))
  }
}

class TestBuilder private(graph: mutable.Graph[TestEvent, DiEdge]) {
  import TestBuilder._

  def this() = this(mutable.Graph())

  /** IOEvent happens independent from any other event
    *
    * The request of passive ports (e.g. activate) will be registered using this here
    */
  def addOrigin[P <: Port](o: IOEvent[P]): TestEvent = {
    graph += o
    o
  }

  /** IOEvent happens always after the given one */
  private def addSubsequentIOEvent[P <: Port](n: TestEvent, s: IOEvent[P]): TestEvent = graph.addAndGet(n ~> s).target

  /** add a succeeding event on a [[SyncPort]] */
  def addSuccessor(n: TestEvent, s: SyncPort): TestEvent = addSubsequentIOEvent(n, IOSyncEvent(s))

  /** add a succeeding event on a [[DataPort]] with a concrete value */
  def addSuccessor(n: TestEvent, d: DataPort, v: Int): TestEvent = addSubsequentIOEvent(n, IODataEvent(d, v))

  /** add a succeeding event on a [[DataPort]] with a [[Variable]] instead of a concrete value */
  def addSuccessor(n: TestEvent, d: DataPort, v: Variable): TestEvent = addSubsequentIOEvent(n, IODataEventPlaceholder(d, v))

  /** returns a nodeId which represents a state where all given nodes are processed */
  def merge(ns: Set[TestEvent]) = {
    val m = new MergeEvent()
    graph ++= ns.map{_ ~> m}
    m
  }

  /** returns another TestBuilder with the exact same internal state
    *
    * When splitting the simulation path on a decision point, i.e. at a case component,
    * the TestGenerator needs to duplicate the TestBuilder in order to be able to generate multiple different Tests,
    * one for each decision which was made.
    *
    * @return another TestBuilder instance
    */
  def duplicate():TestBuilder = new TestBuilder(graph.clone())

  /** instantiate the built TestGraph containing placeholders for [[IODataEvent]]s
    *
    * While simulating for test generation, IOEvents should be recorded even when no concrete values are known yet.
    * This TestBuilder inserted [[IODataEventPlaceholder]] instances in such cases. When all variables are finally bound,
    * the placeholders can be replaced with [[IODataEvent]] instances using this function.
    *
    * @param f a function returning the bound value for a variable
    * @return a test-graph without any placeholders
    */
  def instantiate(f: VariableFixator): immutable.Graph[TestEvent, DiEdge] = {
    // create a map from all nodes to the placeholder replacements,
    // i.e. IODataEvents for Placeholders and identity for all others
    val nodeMap: Map[graph.NodeT, TestEvent] = graph.nodes.map{ x => x -> (x.value match {
      case p: IODataEventPlaceholder => p.toIODataEvent(f)
      case e => e
    })}.toMap

    // replace placeholders in all edge definitions
    val edges: Set[GraphPredef.Param[TestEvent, DiEdge]] = Set() ++ graph.edges.map{
      e => DiEdge(nodeMap(e.source), nodeMap(e.target)).asInstanceOf[GraphPredef.Param[TestEvent, DiEdge]]
    }

    // create a new, immutable graph from the edges and nodes
    immutable.Graph[TestEvent, DiEdge]() ++ (edges ++ nodeMap.values.map(GraphPredef.OuterNode[TestEvent]))
  }
}
