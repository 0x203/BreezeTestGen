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

  /** determines if a port is currently requested or not */
  sealed trait State
  case object Idle extends State
  case object Requested extends State

  /** every Breeze netlist has an Activation port. here it is */
  def Activate = SyncPort(0, 0, "activate", Passive)
}

/** represents a Port of a Breeze Netlist */
sealed abstract class Port {
  def id: Port.Id
  def name: String
  def sense: Port.Sense
  def direction: Port.Direction
  def channelId: Channel.Id
}

/** a Nonput port, i.g. without data channels */
case class SyncPort(id: Port.Id, channelId: Channel.Id, name: String, sense: Port.Sense) extends Port {
  import Port._
  val direction = Port.Nonput

  /** creates a signal for the connected channel */
  def createSignal(): Signal = sense match {
    case Active => Request(channelId)
    case Passive => Acknowledge(channelId)
  }
}

/** an Input or Output port, thus having a bitCount for the data channel and a isSigned flag */
case class DataPort(id: Port.Id,
                    channelId: Channel.Id,
                    name: String,
                    sense: Port.Sense,
                    direction: Port.Direction,
                    bitCount: Int = 8,
                    isSigned: Boolean = false) extends Port {
  import Port._

  require(direction != Nonput)  // this would be a SyncPort

  /** creates a signal for the connected channel */
  def createSignal(value: Data): Signal = (sense, direction) match {
    case (Active, Input) | (Passive, Output) => DataRequest(channelId, value)
    case (Passive, Input) | (Active, Output) => DataAcknowledge(channelId, value)
  }
}
