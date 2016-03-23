package de.hpi.asg.breezetestgen.constraintsolving


/** a container for [[de.hpi.asg.breezetestgen.constraintsolving.Constraint]]s
  * and [[de.hpi.asg.breezetestgen.constraintsolving.Variable]]s.
  *
  */
sealed trait ConstraintVariable
case class ConstraintCV(c: Constraint) extends ConstraintVariable
case class VariableCV(v: Variable) extends ConstraintVariable

object ConstraintVariable {
  implicit def constraint2ConstraintCV(c: Constraint): ConstraintCV = ConstraintCV(c)
  implicit def constraint2VariableCV(v: Variable): VariableCV = VariableCV(v)
}
