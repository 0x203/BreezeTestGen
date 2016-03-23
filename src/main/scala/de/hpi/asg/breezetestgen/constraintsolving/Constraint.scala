package de.hpi.asg.breezetestgen.constraintsolving

/** Superclass of all Constraints which should be re-used across Solver instances
  *
  * Constraint programming libraries come with a lot of Constraints, e.g. [[org.chocosolver.solver.constraints]].
  * However, instances of these are always bound to concrete solver instances. We need to re-use constraints for
  * multiple solver runs. This class serves as a template for constraints instantiated in a new solver run.
  *
   * @param reifyWith an optional [[BoolVariable]] used for reification of this constraint
  */
abstract class Constraint(val reifyWith: Option[BoolVariable]) {
  def asString: String
}
