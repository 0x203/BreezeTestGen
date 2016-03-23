package de.hpi.asg.breezetestgen.constraintsolving


case class TernaryConstraint(result: Variable,
                             a: Variable,
                             op: ArithOperator,
                             b: Either[Variable, Int],
                             override val reifyWith: Option[BoolVariable]
                            ) extends Constraint(reifyWith) {
  val operator = op match {
    case Plus => "+"
    case Minus => "-"
    case And => "&"
    case Or => "|"
  }

  private val rightString = b match {
    case Left(v) => v.name
    case Right(c) => c.toString
  }

  def asString: String = s"${result.name} = ${a.name} $operator $rightString"

}

object TernaryConstraint {
  // overload constructor to avoid needing to write "Left" and "right" all the time
  def apply(result: Variable, a: Variable, op: ArithOperator, b: Variable, rf: BoolVariable): TernaryConstraint =
    TernaryConstraint(result, a, op, Left(b), Option(rf))

  def apply(result: Variable, a: Variable, op: ArithOperator, b: Variable): TernaryConstraint =
    TernaryConstraint(result, a, op, Left(b), None)

  def apply(result: Variable, a: Variable, op: ArithOperator, b: Int, rf: BoolVariable): TernaryConstraint =
    TernaryConstraint(result, a, op, Right(b), Option(rf))

  def apply(result: Variable, a: Variable, op: ArithOperator, b: Int): TernaryConstraint =
    TernaryConstraint(result, a, op, Right(b), None)
}
