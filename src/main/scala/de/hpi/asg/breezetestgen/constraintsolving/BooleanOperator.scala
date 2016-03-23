package de.hpi.asg.breezetestgen.constraintsolving


sealed trait BooleanOperator {
  def opposite: BooleanOperator
}
case object Equals extends BooleanOperator {
  def opposite = NotEquals
}
case object NotEquals extends BooleanOperator {
  def opposite = Equals
}
case object GreaterThan extends BooleanOperator {
  def opposite = LessOrEqual
}
case object LessThan extends BooleanOperator {
  def opposite = GreaterOrEqual
}
case object GreaterOrEqual extends BooleanOperator {
  def opposite = LessThan
}
case object LessOrEqual extends BooleanOperator {
  def opposite = GreaterOrEqual
}
