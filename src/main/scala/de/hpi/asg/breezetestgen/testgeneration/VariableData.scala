package de.hpi.asg.breezetestgen.testgeneration

import constraintsolving._
import de.hpi.asg.breezetestgen.domain.Constant.Underlying
import de.hpi.asg.breezetestgen.domain.{Constant, Data}
import de.hpi.asg.breezetestgen.testgeneration.TestBuilder.VariableFixator


class VariableData(val underlying: Variable, constraintO: Option[Constraint]) extends Data {
  import VariableData._

  def this(underlying: Variable) = this(underlying, None)
  def this(underlying: Variable, constraint: Constraint) = this(underlying, Option(constraint))

  def bitCount = underlying.bitCount
  def isSigned = underlying.isSigned

  def isFalsy = Left(BinaryConstraint(underlying, Equals, 0))
  def isTruthy = Left(BinaryConstraint(underlying, NotEquals, 0))

  def isEqual(o: Data): Data.ConstraintOrBool = Left(o match {
    case c: Constant => BinaryConstraint(underlying, Equals, c.value)
    case vd: VariableData => BinaryConstraint(underlying, Equals, vd.underlying)
  })

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

  def not(): Data = {
    val newUnderlying = Variable(s"!${underlying.name}", bitCount, isSigned)
    val constraint = BitwiseNot(underlying, newUnderlying, None)
    new VariableData(newUnderlying, constraint)
  }
  def selectBits(range: Range): VariableData = {
    require(range.start < bitCount, s"${underlying.name}: selection starts at ${range.start}, just have $bitCount")
    require(range.last < bitCount, s"${underlying.name}: selection ends at ${range.last}, just have $bitCount")
    val newUnderlying = Variable(s"${underlying.name}[${range.start}:${range.end}]", range.size, isSigned = false)
    val constraint = SelectBits(underlying, range.start, range.last, newUnderlying, None)
    new VariableData(newUnderlying, constraint)
  }
  def combineWithMoreSignificant(o: Data): Data = combine(this, o)
  def combineWitLessSignificant(o: Data): Data = combine(o, this)

  def constMinus(o: Constant): VariableData = throw new NoSuchElementException //TODO: implement me
  def xor(o: Data): VariableData = throw new NoSuchElementException //TODO: arithOp(Xor, o)
  def adapt(targetBitCount: Int, targetSigned: Boolean,
            sourceBitCount: Int = bitCount, sourceSigned: Boolean = isSigned): VariableData =
    throw new NoSuchElementException //TODO: implement me

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

  def addToConstraintCollection(cc: ConstraintCollection): ConstraintCollection = constraintO match {
    case None => cc.addVariable(underlying)
    case Some(constraint) => cc.addVariable(underlying).addConstraint(constraint)
  }

  override def toString = s"VariableData($underlying, $constraintO)"
}

private object VariableData {
  private def combine(ls: Data, ms: Data): VariableData = {
    val newUnderlying = Variable(s"concatinated", ls.bitCount + ms.bitCount, ms.isSigned)
    val constraint = (ls, ms) match {
      case (c_ls: Constant, v_ms: VariableData) =>
        Combine(v_ms.underlying, Left(c_ls), aIsLeastSignificant = false, newUnderlying, None)
      case (v_ls: VariableData, c_ms: Constant) =>
        Combine(v_ls.underlying, Left(c_ms), aIsLeastSignificant = true, newUnderlying, None)
      case (v_ls: VariableData, v_ms: VariableData) =>
        Combine(v_ls.underlying, Right(v_ms.underlying), aIsLeastSignificant = true, newUnderlying, None)
      case _ => throw new NoSuchElementException("Impossible to have two constants here")
    }
    new VariableData(newUnderlying, constraint)
  }
}
