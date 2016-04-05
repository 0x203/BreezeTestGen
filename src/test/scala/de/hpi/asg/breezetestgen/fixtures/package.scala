package de.hpi.asg.breezetestgen

import de.hpi.asg.breezetestgen.domain.Netlist

package object fixtures {
  def breezeFilePath = "/breezefiles/gcd.breeze"
  def breezeFile: java.io.File = new java.io.File(getClass.getResource(breezeFilePath).getPath)
  def gcdNetlist(): Netlist = BreezeTransformer.parse(breezeFile).get
}
