package de.hpi.asg.breezetestgen.constraintsolving


case class ConstraintCollection(parent: Option[ConstraintCollection] = None,
                                variables: Set[Variable] = Set.empty,
                                constraints: Set[Constraint] = Set.empty) {
  def addVariable(v: Variable): ConstraintCollection = this.copy(variables = variables + v)
  def addVariables(vs: Traversable[Variable]): ConstraintCollection = this.copy(variables = variables ++ vs)
  def addConstraint(c: Constraint): ConstraintCollection = this.copy(constraints= constraints + c)
  def addConstraints(cs: Traversable[Constraint]): ConstraintCollection = this.copy(constraints= constraints ++ cs)
  def add(cvs: Traversable[ConstraintVariable]): ConstraintCollection = this.copy(
      constraints = constraints ++ cvs.collect{case c: ConstraintCV => c.c},
      variables = variables ++ cvs.collect{case v: VariableCV => v.v}
  )

  def allVariables: Set[Variable] = parent.map(_.variables).getOrElse(Set.empty) ++ variables
  def allConstraints: Set[Constraint] =  parent.map(_.constraints).getOrElse(Set.empty) ++ constraints

  def fork(): ConstraintCollection = ConstraintCollection(Some(this))
}
