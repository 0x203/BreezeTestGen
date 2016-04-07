package de.hpi.asg.breezetestgen.domain

object Constant {
  type Underlying = Int
}


case class Constant(v: Constant.Underlying, bitCount: Int = 8, isSigned: Boolean = false) extends Data {
  val value =  v % (maxValue + 1)

  def selectBits(range: Range): Data = {
    val shifted: Constant.Underlying = value >> range.min
    // max of range gets automatically cut off due bitCount
    Constant(shifted, range.length, isSigned = false)
  }

  def isTruthy = Right(value != 0)
  def isFalsy = Right(value == 0)

  def plus(o: Data): Data = o.plusConst(this)
  def minus(o: Data): Data = o.constMinus(this)

  def equals(o: Data): Data = o.equalsConst(this)
  def equalsNot(o: Data): Data = o.equalsNotConst(this)

  def lessThen(o: Data): Data = o.greaterThenConst(this)
  def greaterThen(o: Data): Data = o.lessThenConst(this)
  def lessOrEqual(o: Data): Data = o.greaterOrEqualConst(this)
  def greaterOrEqual(o: Data): Data = o.lessOrEqualConst(this)

  def and(o: Data): Data = o.andConst(this)
  def or(o: Data): Data = o.orConst(this)
  def xor(o: Data): Data = o.xorConst(this)

  def not(): Data = {
    require(isSigned)
    Constant(~value)
  }
  override def plusConst(o: Constant): Constant = Constant(value + o.value)
  override def minusConst(o: Constant): Constant = Constant(value - o.value)
  override def constMinus(o: Constant): Constant = Constant(o.value - value)
  override def equalsConst(o: Constant): Constant = bool2Constant(value == o.value)
  override def equalsNotConst(o: Constant): Constant = bool2Constant(value != o.value)
  override def lessThenConst(o: Constant): Constant = bool2Constant(value < o.value)
  override def greaterThenConst(o: Constant): Constant = bool2Constant(value > o.value)
  override def lessOrEqualConst(o: Constant): Constant = bool2Constant(value <= o.value)
  override def greaterOrEqualConst(o: Constant): Constant = bool2Constant(value >= o.value)
  override def andConst(o: Constant): Constant = Constant(value & o.value)
  override def orConst(o: Constant): Constant = Constant(value | o.value)
  override def xorConst(o: Constant): Constant = Constant(value ^ o.value)

  private def bool2Constant(b: Boolean): Constant = Constant(if (b) 1 else 0, 1, isSigned=false)
}
