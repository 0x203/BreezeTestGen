package de.hpi.asg.breezetestgen

import de.hpi.asg.breezetestgen.domain.components.{BrzComponent, HandshakeComponent}
import de.hpi.asg.breezetestgen.domain._
import de.uni_potsdam.hpi.asg.common.breeze.model._

/** takes some netlist definition objects from asg.common and transform it to our domain objects */
object BreezeTransformer {
  //TODO: transform hierarchical netlists, too

  def parse(breezeFile: java.io.File): Option[Netlist] =
    Option(
      // TODO: verify hard coded path works in packaged version
      BreezeProject.create(breezeFile, "src/main/resources/components.xml", false, false)
    ).flatMap(transformProject)

  def transformProject(breezeProject: BreezeProject): Option[Netlist] =
    breezeProject.getSortedNetlists.toArray().
      lastOption.collectFirst{case a: AbstractBreezeNetlist=> a}.map(transformNetlist)

  def transformNetlist(baseNetlist: AbstractBreezeNetlist): Netlist = {

    val id = Netlist.nextId
    val ports: Map[Port.Id, Port] = extractPorts(baseNetlist)

    val channels: Map[Channel.Id, Channel[Channel.Endpoint]] = extractChannels(baseNetlist)
    val components: Map[HandshakeComponent.Id, BrzComponent] = extractComponents(baseNetlist)

    Netlist(id, ports, channels, components)
  }

  private def extractPorts(netlist: AbstractBreezeNetlist): Map[Port.Id, Port] = {
    import scala.collection.JavaConversions.collectionAsScalaIterable

    netlist.getAllPorts.map(extractPort).map{case p => p.id -> p}.toMap
  }

  private def extractPort(raw: PortComponent): Port = {
    import scala.collection.JavaConversions.collectionAsScalaIterable
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

  private def extractChannels(netlist: AbstractBreezeNetlist): Map[Channel.Id, Channel[Channel.Endpoint]] = {
    import collection.JavaConversions.collectionAsScalaIterable
    netlist.getChannelList.values.map(extractChannel).map{case c => c.id -> c}.toMap
  }

  private def extractChannel(raw: HSChannel): Channel[Channel.Endpoint] = {
    val id: Channel.Id = raw.getId
    val active: Channel.Endpoint = asEndpoint(raw.getActive)
    val passive: Channel.Endpoint = asEndpoint(raw.getPassive)

    if (raw.getDatawidth == 0)
      SyncChannel(id, active, passive)
    else raw.getDatatype match {
      case HSChannel.DataType.pull => PullChannel(id, active, passive)
      case HSChannel.DataType.push => PullChannel(id, active, passive)
    }
  }

  private def asEndpoint(compInst: ComponentInst): Channel.Endpoint = compInst match {
    case port: PortComponent => Channel.PortEndpoint(port.getId)
    case hsComp: HSComponentInst => Channel.CompEndpoint(hsComp.getId)
    case subNetlist: BreezeNetlistInst => throw new RuntimeException("Did not expect a subNetlist as target of channel")
  }

  private def extractComponents(netlist: AbstractBreezeNetlist): Map[HandshakeComponent.Id, BrzComponent] = {
    import collection.JavaConversions.collectionAsScalaIterable
    netlist.getAllHSInstances.map{ComponentExtractors.extract(_)}.map{case c => c.id -> c}.toMap
  }
}

object ComponentExtractors {
  import components.brzcomponents._

  type Extractor = HSComponentInst => Option[BrzComponent]

  def extract(implicit raw: HSComponentInst): BrzComponent = {
    raw.getBrzStr match {
      case "BrzCallMux" => new CallMux(id, channelSet(0), channel(1))
      case "BrzCase" => new Case(id, CaseSpecParser.fromString(stringParam(2)), channel(0), channelSeq(1))
      case "BrzConcur" => new Concur(id, channel(0), channelSet(1))
      case "BrzBinaryFunc" => new BinaryFunc(id, stringParam(3), channel(0), channel(1), channel(2))
      case "BrzBinaryFuncConstR" =>
        new BinaryFuncConstR(id, stringParam(3),
          Constant(intParam(7), intParam(2), boolParam(6)),
          channel(0), channel(1)
        )
      case "BrzFetch" => new Fetch(id, channel(0), channel(1), channel(2))
      case "BrzSequence" => new Sequence(id, channel(0), channelSeq(1))
      case "BrzVariable" =>
        new Variable(id, stringParam(2), VariableReaderSpec.fromString(stringParam(3)), channel(0), channelSeq(1))
      case "BrzWhile" => new While(id, channel(0), channel(1), channel(2))
      case unknown => throw new RuntimeException(s"Unknown component with name: $unknown")
    }
  }

  /** extracts the id from an implicit HSComponentInst */
  private def id(implicit raw: HSComponentInst): HandshakeComponent.Id = raw.getId

  /** returns the single channel at the given parameter position from an implicit HSComponentInst */
  private def channel(i: Int)(implicit raw: HSComponentInst): Channel.Id = {
    raw.getChan(i).ensuring{_.size == 1}.get(0).getId
  }

  /** returns a set of channels at the given parameter position from an implicit HSComponentInst */
  private def channelSet(i: Int)(implicit raw: HSComponentInst): Set[Channel.Id] = {
    import collection.JavaConversions.collectionAsScalaIterable
    raw.getChan(i).map{_.getId}.toSet
  }

  /** returns a seq of channels at the given parameter position from an implicit HSComponentInst */
  private def channelSeq(i: Int)(implicit raw: HSComponentInst): Seq[Channel.Id] = {
    import collection.JavaConversions.collectionAsScalaIterable
    raw.getChan(i).map{_.getId}.toSeq
  }

  /** returns a String at the given parameter position from an implicit HSComponentInst */
  private def stringParam(i: Int)(implicit raw: HSComponentInst): String = {
    raw.getBechans.get(i) match {
      case s: java.lang.String => s.stripPrefix("\"").stripSuffix("\"")
      case x => throw new RuntimeException(s"bechan #$i is no String: $x")
    }
  }

  /** returns a Int at the given parameter position from an implicit HSComponentInst */
  private def intParam(i: Int)(implicit raw: HSComponentInst): Int = {
    raw.getBechans.get(i) match {
      case i: java.lang.Integer => i
      case x => throw new RuntimeException(s"bechan #$i is no Integer: $x")
    }
  }

  /** returns a Boolean at the given parameter position from an implicit HSComponentInst */
  private def boolParam(i: Int)(implicit raw: HSComponentInst): Boolean = {
    raw.getBechans.get(i) match {
      case b: java.lang.Boolean => b
      case x => throw new RuntimeException(s"bechan #$i is no Boolean: $x")
    }
  }

  /** converts Strings like ";1..3;0..15" to something like (0->({Empty Range}), 1->(1 to 3), 2->(0 to 15)) */
  private object VariableReaderSpec {
    private object StringIndex {
      def apply(i: Int) = i.toString
      def unapply(s: String) = {
        try {
          Some(s.toInt)
        } catch {
          case _: java.lang.NumberFormatException => None
        }
      }
    }

    private object StringRange {
      def apply(start: Int, end: Int) = start.toString + ".." + end.toString
      def unapply(s: String): Option[Range] = {
        s.split("\\.\\.", 2) match {  // need escaping because its a regex in a string
          case Array(StringIndex(start), StringIndex(end)) => Some(start to end)
          case Array("") => Some(0 until 0)  // empty Range
          case _ => None
        }

      }
    }

    def fromString(specification: String): Variable.ReaderSpec = {
      if (specification == "") return {i: Int => Some(0 until 0)}   // variables without selector always return everything

      val ranges: Map[Int, Range] = specification.split(";").zipWithIndex.map{
        case (StringRange(range), index) => index -> range
        case (x, index) => throw new RuntimeException(s"cannot parse Variable specification part: $x")
      }.toMap

      ranges.get
    }
  }

  /** converts Strings like "0;1..3;4" to something like (0->0, 1->1, 2->1, 3->1, 4->2) */
  private object CaseSpecParser {
    private object StringIndex {
      def apply(i: Int) = i.toString
      def unapply(s: String) = {
        try {
          Some(s.toInt)
        } catch {
          case _: java.lang.NumberFormatException => None
        }
      }
    }

    private object StringRange {
      def apply(start: Int, end: Int) = start.toString + ".." + end.toString
      def unapply(s: String) = {
        s.split("\\.\\.", 2) match {  // need escaping because its a regex in a string
          case Array(StringIndex(start), StringIndex(end)) => Some(start to end)
          case _ => None
        }

      }
    }

    def fromString(specification: String): Case.SelectorSpec = {
      val indexes = collection.mutable.ListMap.empty[Int, Int]
      val ranges = collection.mutable.ListBuffer.empty[(Range, Int)]
      for ((number, index) <- specification.split(";").zipWithIndex) number match {
        case StringIndex(i) => indexes += (i -> index)
        case StringRange(r) => ranges += ((r, index))
      }

      { inp: Int =>
        indexes.get(inp) orElse {
          ranges.collectFirst { case (r, i) if r contains inp => i }
        }
      }
    }
  }
}
