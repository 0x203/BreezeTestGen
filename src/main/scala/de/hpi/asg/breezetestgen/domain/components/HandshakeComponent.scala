package de.hpi.asg.breezetestgen.domain.components

object HandshakeComponent {
  //case class Id(netlists: List[de.hpi.asg.breezetestgen.domain.Netlist.Id], own: Int)
  type Id = List[Int]

  case class State[C, D](controlState: C, dataState: D)
}

trait HandshakeComponent {
  def id: HandshakeComponent.Id
}
