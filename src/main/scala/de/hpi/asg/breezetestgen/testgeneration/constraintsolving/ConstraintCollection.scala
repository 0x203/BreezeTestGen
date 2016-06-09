package de.hpi.asg.breezetestgen.testgeneration.constraintsolving


case class ConstraintCollection(parent: Option[ConstraintCollection] = None,
                                variables: Set[Variable] = Set.empty,
                                constraints: Set[Constraint] = Set.empty) {
  def addVariable(v: Variable): ConstraintCollection =
    if (allVariables contains v) {
      this
    } else {
      this.copy(variables = variables + v)
    }

  def addConstraint(c: Constraint): ConstraintCollection =
    if (allConstraints contains c) {
      this
    } else {
      this.copy(constraints= constraints + c)
    }

  def add(cvs: Traversable[ConstraintVariable]): ConstraintCollection = this.copy(
      constraints = cvs.collect{case c: ConstraintCV => c.c}.toSet -- allConstraints ++ constraints ,
      variables = cvs.collect{case v: VariableCV => v.v}.toSet -- allVariables ++ variables
  )

  def allVariables: Set[Variable] = parent.map(_.allVariables).getOrElse(Set.empty) ++ variables
  def allConstraints: Set[Constraint] =  parent.map(_.allConstraints).getOrElse(Set.empty) ++ constraints

  def fork(): ConstraintCollection = ConstraintCollection(Some(this))
}
