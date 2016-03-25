package de.hpi.asg.breezetestgen

import de.hpi.asg.breezetestgen.domain.Netlist
import de.uni_potsdam.hpi.asg.common.breeze.model.{AbstractBreezeNetlist, BreezeProject}


object BreezeTransformer {
  def parse(breezeFile: java.io.File): Option[Netlist] =
    Option(
      // TODO: verify hard coded path works in packaged version
      BreezeProject.create(breezeFile, "src/main/resources/components.xml", false, false)
    ).flatMap(transformProject)

  def transformProject(breezeProject: BreezeProject): Option[Netlist] =
    breezeProject.getSortedNetlists.toArray().
      lastOption.collectFirst{case a: AbstractBreezeNetlist=> a}.map(transfromNetlist)

  def transfromNetlist(mainNetlist: AbstractBreezeNetlist): Netlist = {
    mainNetlist.asInstanceOf[Netlist]
  }
}
