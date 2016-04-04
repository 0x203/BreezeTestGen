package de.hpi.asg.breezetestgen.testgeneration


import de.hpi.asg.breezetestgen.constraintsolving.Variable
import de.hpi.asg.breezetestgen.domain.{Constant, DataAcknowledge, DataRequest}
import de.hpi.asg.breezetestgen.testing._

import scalax.collection._
import scalax.collection.GraphEdge._


object TestBuilder {
  /** a function used to fixate a variable to a concrete value */
  type VariableFixator = Variable => Constant
}

class TestBuilder private(graph: mutable.Graph[TestEvent, DiEdge]) {
  import TestBuilder._

  def this() = this(mutable.Graph())

  /** IOEvent happens independent from any other event
    *
    * The request of passive ports (e.g. activate) will be registered using this here
    */
  def addOrigin(o: IOEvent): TestEvent = {
    graph += o
    o
  }

  /** IOEvent happens always after the given one */
  def addSuccessor(n: TestEvent, s: IOEvent): TestEvent = graph.addAndGet(n ~> s).target

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

  /** instantiate the built TestGraph containing placeholders for Variables in DataSignals
    * ([[de.hpi.asg.breezetestgen.domain.DataAcknowledge]] and [[de.hpi.asg.breezetestgen.domain.DataRequest]])
    *
    * @param f a function returning the bound value for a variable
    * @return a test-graph without any placeholders
    */
  def instantiate(f: VariableFixator): Test = {
    // create a map from all nodes to the placeholder replacements,
    // i.e. IODataEvents for Placeholders and identity for all others
    val nodeMap: Map[graph.NodeT, TestEvent] = graph.nodes.map{ x => x -> (x.value match {
      case IOEvent(x @ DataRequest(_, v: Variable)) => IOEvent(x.copy(data = f(v)))
      case IOEvent(x @ DataAcknowledge(_, v: Variable)) => IOEvent(x.copy(data = f(v)))
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
