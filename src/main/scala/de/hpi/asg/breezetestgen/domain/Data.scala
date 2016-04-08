package de.hpi.asg.breezetestgen.domain

import de.hpi.asg.breezetestgen.testgeneration.constraintsolving.Constraint

trait Data {
  def bitCount: Int
  def isSigned: Boolean

  def isFalsy: Data.ConstraintOrBool
  def isTruthy: Data.ConstraintOrBool

  def selectBits(range: Range): Data

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

  def plusConst(o: Constant): Data = plus(o)
  def minusConst(o: Constant): Data = minus(o)
  def equalsConst(o: Constant): Data = equals(o)
  def equalsNotConst(o: Constant): Data = equalsNot(o)
  def lessThenConst(o: Constant): Data = lessThen(o)
  def greaterThenConst(o: Constant): Data = greaterThen(o)
  def lessOrEqualConst(o: Constant): Data = lessOrEqual(o)
  def greaterOrEqualConst(o: Constant): Data = greaterOrEqual(o)
  def andConst(o: Constant): Data = and(o)
  def orConst(o: Constant): Data = or(o)
  def xorConst(o: Constant): Data = xor(o)

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

object Data {
  type ConstraintOrBool = Either[Constraint, Boolean]
}
