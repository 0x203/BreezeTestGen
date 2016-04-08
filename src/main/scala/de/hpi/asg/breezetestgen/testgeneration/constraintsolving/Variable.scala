package de.hpi.asg.breezetestgen.testgeneration.constraintsolving


class Variable(val name: String, val bitCount: Int, val isSigned: Boolean) {
  private val domainSize: Int =  math.pow(2, bitCount).toInt
  val minValue: Int = if (isSigned) -domainSize/2 else 0
  val maxValue: Int = if (isSigned) domainSize/2 - 1 else domainSize - 1
  override def toString = s"Variable($name, $bitCount, $isSigned)"
}

object Variable {
  def apply(name: String, bitCount: Int, isSigned: Boolean): Variable = new Variable(name, bitCount, isSigned)
  def unapply(v: Variable): Option[(String, Int, Boolean)] = Some(v.name, v.bitCount, v.isSigned)
}


case class BoolVariable(override val name: String) extends Variable(name, 1, isSigned = false) {
  def isFalse: Constraint = BinaryConstraint(this, Equals, 0)
  def isTrue: Constraint = BinaryConstraint(this, NotEquals, 0)

  override def toString = s"BoolVariable($name)"
}
