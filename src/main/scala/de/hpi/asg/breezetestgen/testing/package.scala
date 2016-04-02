package de.hpi.asg.breezetestgen

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.immutable.Graph

package object testing {
  type Test = Graph[TestEvent, DiEdge]
}
