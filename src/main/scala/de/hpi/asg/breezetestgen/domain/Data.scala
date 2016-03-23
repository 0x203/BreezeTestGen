package de.hpi.asg.breezetestgen.domain

trait Data {
  def bitCount: Int
  def isSigned: Boolean

  private val domainSize: Int =  math.pow(2, bitCount).toInt
  val minValue: Int = if (isSigned) -domainSize/2 else 0
  val maxValue: Int = if (isSigned) domainSize/2 - 1 else domainSize - 1

  def plus(o: Data): Data
  def minus(o: Data): Data
  def equals(o: Data): Data
  def equalsNot(o: Data): Data
  def lessThen(o: Data): Data
  def greaterThen(o: Data): Data
  def lessOrEqual(o: Data): Data
  def greaterOrEqual(o: Data): Data
  def and(o: Data): Data
  def or(o: Data): Data
  def xor(o: Data): Data
  def not(): Data

  def plusConst(o: Constant): Data
  def minusConst(o: Constant): Data
  def equalsConst(o: Constant): Data
  def equalsNotConst(o: Constant): Data
  def lessThenConst(o: Constant): Data
  def greaterThenConst(o: Constant): Data
  def lessOrEqualConst(o: Constant): Data
  def greaterOrEqualConst(o: Constant): Data
  def andConst(o: Constant): Data
  def orConst(o: Constant): Data
  def xorConst(o: Constant): Data

  def constMinus(o: Constant): Data // this is needed for double dispatch

  def +(o: Data): Data = plus(o)
  def -(o: Data): Data = minus(o)
  def ===(o: Data): Data = equals(o)
  def !==(o: Data): Data = equalsNot(o)
  def <(o: Data): Data = lessThen(o)
  def >(o: Data): Data = greaterThen(o)
  def <=(o: Data): Data = lessOrEqual(o)
  def >=(o: Data): Data = greaterOrEqual(o)
  def &(o: Data): Data = and(o)
  def |(o: Data): Data = or(o)
  def ^(o: Data): Data = xor(o)
  def !(): Data = not()
}
