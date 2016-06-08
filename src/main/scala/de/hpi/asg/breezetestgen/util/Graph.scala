package de.hpi.asg.breezetestgen.util

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.immutable.Graph
import scalax.collection.{GraphPredef, GraphTraversal}

object Graph {
  def mapNodes[N1, N2](origin: GraphTraversal[N1, DiEdge], f: N1 => N2): Graph[N2, DiEdge] = {
    val nodeMap = origin.nodes
      .map(x => x -> f(x))
      .toMap

    // replace placeholders in all edge definitions
    val edges: Set[GraphPredef.Param[N2, DiEdge]] = Set() ++ origin.edges.map{
      e => DiEdge(nodeMap(e.source), nodeMap(e.target)).asInstanceOf[GraphPredef.Param[N2, DiEdge]]
    }

    // create a new, immutable graph from the edges and nodes
    scalax.collection.immutable.Graph[N2, DiEdge]() ++ (edges ++ nodeMap.values.map(GraphPredef.OuterNode[N2]))
  }
}
