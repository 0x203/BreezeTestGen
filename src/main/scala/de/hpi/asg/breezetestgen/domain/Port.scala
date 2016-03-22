package de.hpi.asg.breezetestgen.domain

object Port {
  type Id = Int

  /** determines who initiates communication */
  sealed trait Sense
  case object Active extends Sense
  case object Passive extends Sense

  /** determines the direction of data flow */
  sealed trait Direction
  sealed trait DataDirection
  case object Input extends Direction with DataDirection
  case object Output extends Direction with DataDirection
  case object Nonput extends Direction

  /** every Breeze netlist has an Activation port. here it is */
  def Activate = SyncPort(0, "activate", Passive)
}

/** represents a Port of a Breeze Netlist */
sealed abstract class Port(id: Port.Id, name: String, sense: Port.Sense, direction: Port.Direction)

/** a Nonput port, i.g. without data channels */
case class SyncPort(id: Port.Id,
                    name: String,
                    sense: Port.Sense)
  extends Port(id, name, sense, Port.Nonput)

/** an Input or Output port, thus having a bitCount for the data channel and a isSigned flag */
case class DataPort(id: Port.Id,
                    name: String,
                    sense: Port.Sense,
                    direction: Port.Direction,
                    bitCount: Int = 8,
                    isSigned: Boolean = false)
  extends Port(id, name, sense, direction)
