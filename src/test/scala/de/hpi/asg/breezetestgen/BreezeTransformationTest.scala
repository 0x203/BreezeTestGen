package de.hpi.asg.breezetestgen

import de.hpi.asg.breezetestgen.domain.{Channel, Port, SyncChannel, components}
import fixtures._

/** checks weather a parsed .breeze-file is correctly transformed to a netlist */
class BreezeTransformationTest extends baseclasses.UnitTest {
  val breezeFilePath = "/breezefiles/gcd.breeze"
  val breezeFile: java.io.File = new java.io.File(getClass.getResource(breezeFilePath).getPath)

  "The BreezeTransformer" should "read from a .breeze-file and create a netlist" in {
    val netlistOption = BreezeTransformer.parse(breezeFile)
    assert(netlistOption.isDefined)
  }

  it should "have correct number of ports, channels and components" in {
    val netlist = gcdNetlist()
    assert(netlist.ports.size == 4)
    assert(netlist.channels.size == 43)
    assert(netlist.components.size == 27)
  }

  it should "have correct ports" in {
    val ports = gcdNetlist().ports

    for ((id, port) <- ports) assert(id == port.id)

    val activate = ports(-1)
    assert(activate.name == "activate")
    assert(activate.sense == Port.Active)
    assert(activate.direction == Port.Nonput)

    val ain = ports(-2)
    assert(ain.name == "ain")
    assert(ain.sense == Port.Passive)
    assert(ain.direction == Port.Input)

    val bin = ports(-3)
    assert(bin.name == "bin")
    assert(bin.sense == Port.Passive)
    assert(bin.direction == Port.Input)

    val o = ports(-4)
    assert(o.name == "o")
    assert(o.sense == Port.Passive)
    assert(o.direction == Port.Output)
  }

  it should "have correct channels" in {
    val netlist = gcdNetlist()

    for ((id, channel) <- netlist.channels) assert(id == channel.id)

    val activateChannel = netlist.channels(1)
    assert(activateChannel.isInstanceOf[SyncChannel[Channel.Endpoint]])
    assert(activateChannel.active == Channel.PortEndpoint(-1))
    assert(activateChannel.passive == Channel.CompEndpoint(4))

    // Test other channels, too?
  }

  it should "have correct components" in {
    val netlist = gcdNetlist()

    for ((id, component) <- netlist.components) assert(id == component.id)

    val doWhile= netlist.components(8)
    assert(doWhile.isInstanceOf[components.brzcomponents.While])

    val aVar = netlist.components(0)
    assert(aVar.isInstanceOf[components.brzcomponents.Variable])

    // Test other components, too?
  }
}
