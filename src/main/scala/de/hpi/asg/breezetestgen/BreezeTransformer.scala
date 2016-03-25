package de.hpi.asg.breezetestgen

import de.hpi.asg.breezetestgen.domain.components.{BrzComponent, HandshakeComponent}
import de.hpi.asg.breezetestgen.domain._
import de.uni_potsdam.hpi.asg.common.breeze.model.{AbstractBreezeNetlist, BreezeProject, PortComponent}


object BreezeTransformer {
  def parse(breezeFile: java.io.File): Option[Netlist] =
    Option(
      // TODO: verify hard coded path works in packaged version
      BreezeProject.create(breezeFile, "src/main/resources/components.xml", false, false)
    ).flatMap(transformProject)

  def transformProject(breezeProject: BreezeProject): Option[Netlist] =
    breezeProject.getSortedNetlists.toArray().
      lastOption.collectFirst{case a: AbstractBreezeNetlist=> a}.map(transfromNetlist)

  def transfromNetlist(baseNetlist: AbstractBreezeNetlist): Netlist = {

    val id = Netlist.nextId
    val ports: Map[Port.Id, Port] = extractPorts(baseNetlist)

    val channels: Map[Channel.Id, Channel[Channel.Endpoint]] = Map.empty
    val components: Map[HandshakeComponent.Id, BrzComponent] = Map.empty


    Netlist(id, ports, channels, components)
  }

  private def extractPorts(netlist: AbstractBreezeNetlist): Map[Port.Id, Port] = {
    import scala.collection.JavaConversions.asScalaSet

    netlist.getAllPorts.map(extractPort).map{case p => p.id -> p}.toMap
  }

  private def extractPort(raw: PortComponent): Port = {
    import scala.collection.JavaConversions.asScalaBuffer
    val id: Port.Id = raw.getId
    val name = raw.getName

    val gathering = Set() ++ raw.getControlIn ++ raw.getControlOut ++ raw.getDataIn ++ raw.getDataOut
    require(gathering.size == 1)
    val channel = gathering.head
    val channelId: Channel.Id = channel.getId

    val sense: Port.Sense = channel.getActive match {
      case _: PortComponent => Port.Active
      case _ => Port.Passive
    }

    if (channel.getDatawidth == 0) {
      SyncPort(id, channelId, name, sense)
    } else {
      val direction: Port.Direction = raw.getDirection match {
        case PortComponent.Direction.in => Port.Input
        case PortComponent.Direction.out => Port.Output
      }
      // TODO: can we find out if data isSigned?
      DataPort(id, channelId, name, sense, direction, channel.getDatawidth)
    }
  }
}
