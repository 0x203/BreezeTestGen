package de.hpi.asg.breezetestgen.constraintsolving


case class BinaryConstraint(a: Variable,
                            op: BooleanOperator,
                            b: Either[Variable, Int],
                            override val reifyWith: Option[BoolVariable]
                           ) extends Constraint(reifyWith) {
  val operator = op match {
    case Equals => "="
    case NotEquals => "!="
    case GreaterThan => ">"
    case LessThan => "<"
    case GreaterOrEqual => ">="
    case LessOrEqual => "<="
  }

  private val rightString = b match {
    case Left(v) => v.name
    case Right(c) => c.toString
  }

  def asString: String = s"${a.name} $operator $rightString"

  def opposite(): BinaryConstraint = this.copy(op = op.opposite)
}

object BinaryConstraint {
  // overload constructor to avoid needing to write "Left" and "right" all the time
  def apply(a: Variable, op: BooleanOperator, b: Variable, rf: BoolVariable): BinaryConstraint =
    BinaryConstraint(a, op, Left(b), Option(rf))
  def apply(a: Variable, op: BooleanOperator, b: Variable): BinaryConstraint = BinaryConstraint(a, op, Left(b), None)
  def apply(a: Variable, op: BooleanOperator, b: Int, rf: BoolVariable): BinaryConstraint =
    BinaryConstraint(a, op, Right(b), Option(rf))
  def apply(a: Variable, op: BooleanOperator, b: Int): BinaryConstraint = BinaryConstraint(a, op, Right(b), None)
}
