package de.hpi.asg.breezetestgen.testgeneration

import constraintsolving._
import de.hpi.asg.breezetestgen.domain.{Constant, Data}


class VariableData(private[testgeneration] val underlying: Variable,
                   private[testgeneration] val constraint: Constraint) extends Data {

  def bitCount = underlying.bitCount
  def isSigned = underlying.isSigned

  def isFalsy = Left(BinaryConstraint(underlying, Equals, 0))
  def isTruthy = Left(BinaryConstraint(underlying, NotEquals, 0))

  def plus(o: Data): VariableData = arithOp(Plus, o)
  def minus(o: Data): VariableData = arithOp(Minus, o)
  def and(o: Data): VariableData = arithOp(And, o)
  def or(o: Data): VariableData = arithOp(Or, o)
  def equals(o: Data): VariableData = booleanOp(Equals, o)

  def equalsNot(o: Data): VariableData = booleanOp(NotEquals, o)
  def lessThen(o: Data): VariableData = booleanOp(LessThan, o)
  def greaterThen(o: Data): VariableData = booleanOp(GreaterThan, o)
  def lessOrEqual(o: Data): VariableData = booleanOp(LessOrEqual, o)
  def greaterOrEqual(o: Data): VariableData = booleanOp(GreaterOrEqual, o)

  def not(): Data = throw new NoSuchElementException  //TODO: implement me
  def selectBits(range: Range): VariableData = throw new NoSuchElementException  //TODO: implement me
  def constMinus(o: Constant): VariableData = throw new NoSuchElementException //TODO: implement me
  def xor(o: Data): VariableData = throw new NoSuchElementException //TODO: arithOp(Xor, o)

  /** creates new [[VariableData]] with an underlying [[Variable]] which reifies the according arithmetic operation */
  private def arithOp(op: ArithOperator, o: Data): VariableData = {
    val (otherName, other): (String, Either[Variable, Int]) = o match {
      case c: Constant => (c.value.toString, Right(c.value))
      case vd: VariableData => (vd.underlying.name, Left(vd.underlying))
    }
    val newUnderlying = Variable(s"${underlying.name} $op $otherName", bitCount, isSigned)
    val newConstraint = TernaryConstraint(newUnderlying, underlying, op, other, None)

    new VariableData(
      newUnderlying,
      newConstraint
    )
  }

  /** creates new [[VariableData]] with an underlying [[BoolVariable]] which reifies a {this op other}-constraint */
  private def booleanOp(op: BooleanOperator, other: Data): VariableData = {
    val unreifiedConstraint = other match {
      case c: Constant => BinaryConstraint(underlying, op, c.value)
      case vd: VariableData => BinaryConstraint(underlying, op, vd.underlying)
    }

    val boolVariable = BoolVariable(unreifiedConstraint.toString) // creates nicely formatted string name
    val constraint = unreifiedConstraint.copy(reifyWith = Option(boolVariable))

    new VariableData(
      boolVariable,
      constraint
    )
  }
}
