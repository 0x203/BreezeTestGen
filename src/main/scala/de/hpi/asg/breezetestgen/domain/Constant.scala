package de.hpi.asg.breezetestgen.domain

object Constant {
  type Underlying = Int

  private def bool2Constant(b: Boolean): Constant = Constant(if (b) 1 else 0, 1, isSigned=false)
  private def combine(ls: Constant, ms: Constant): Constant = {
    require(!ms.isSigned)
    Constant((ms.value << ls.bitCount) + ls.value, ls.bitCount + ms.bitCount, ms.isSigned)
  }
}


case class Constant(v: Constant.Underlying, bitCount: Int = 8, isSigned: Boolean = false) extends Data {
  import Constant._
  val value =  v % (maxValue + 1)

  def selectBits(range: Range): Data = {
    val shifted: Constant.Underlying = value >> range.min
    // max of range gets automatically cut off due bitCount
    Constant(shifted, range.length, isSigned = false)
  }

  def adapt(targetBitCount: Int, targetSigned: Boolean,
            sourceBitCount: Int = bitCount, sourceSigned: Boolean = isSigned): Data = {
    val newValue =
      if(sourceSigned & !targetSigned)
        math.abs(value)
      else
        value
    Constant(newValue, targetBitCount, targetSigned)
  }

  def isTruthy = Right(value != 0)
  def isFalsy = Right(value == 0)

  def isEqual(o: Data): Data.ConstraintOrBool = o.isEqualConst(this)
  override def isEqualConst(o: Constant): Data.ConstraintOrBool = Right(o.value == value)

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

  def combineWitLessSignificant(o: Data): Data = o.combineWithMoreSignificantConst(this)
  def combineWithMoreSignificant(o: Data): Data = o.combineWitLessSignificant(this)

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
  override def combineWithMoreSignificantConst(o: Constant): Data = combine(this, o)
  override def combineWitLessSignificantConst(o: Constant): Data = combine(o, this)

  def asBitArray: Seq[Int] = {
    val binaryValue = value.toBinaryString.map(_.asDigit).reverse
    Seq.fill(bitCount - binaryValue.length){0} ++ binaryValue
  }
}
