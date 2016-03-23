package de.hpi.asg.breezetestgen.constraintsolving


case class ConstraintCollection(parent: Option[ConstraintCollection] = None,
                                variables: Set[Variable] = Set.empty,
                                constraints: Set[Constraint] = Set.empty) {
  def addVariable(v: Variable): ConstraintCollection = this.copy(variables = variables + v)
  def addVariables(vs: Traversable[Variable]): ConstraintCollection = this.copy(variables = variables ++ vs)
  def addConstraint(c: Constraint): ConstraintCollection = this.copy(constraints= constraints + c)
  def addConstraints(cs: Traversable[Constraint]): ConstraintCollection = this.copy(constraints= constraints ++ cs)

  def allVariables: Set[Variable] = parent match {
    case Some(p) => p.allVariables ++ variables
    case None => variables
  }

  def allConstraints: Set[Constraint] = parent match {
    case Some(p) => p.allConstraints ++ constraints
    case None => constraints
  }

  def fork(): ConstraintCollection = ConstraintCollection(Some(this))
}
