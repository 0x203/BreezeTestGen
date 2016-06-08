package de.hpi.asg.breezetestgen.testgeneration.constraintsolving

import de.hpi.asg.breezetestgen.testgeneration.TestBuilder
import de.hpi.asg.breezetestgen.testing.Test

/** take a constraint collection and a "raw" [[de.hpi.asg.breezetestgen.testgeneration.TestBuilder]]
  * create one or more tests out of it
   */
object TestInstantiator {
  /** return the first working test if any exists */
  def first(cc: ConstraintCollection, tb: TestBuilder): Option[Test] = {
    val solver = new ChocoSolver(cc)
    if(solver.isFeasible) {
      Option(tb.instantiate(solver.next.apply))
    } else None
  }

  /** return a randomly selected test, if any exists */
  def random(cc: ConstraintCollection, tb: TestBuilder): Option[Test] = {
    val solver = new ChocoSolver(cc)
    if(solver.isFeasible) {
      val chosen = choose(solver, randomGenerator)

      Option(tb.instantiate(chosen))
    } else None
  }

  private val randomGenerator = new util.Random()

  /** select a random element from a iterator of unknown length
    * works by replacing the ith element with probability 1/i with the currently chosen one
    *
    * Examples:
    *   length 1: 1/1 for first
    *   length 2: 1/2 for first, 1/2 for second
    *   and so on
    */
  private def choose[A](it: Iterator[A], r: util.Random): A =
  it.zip(Iterator.iterate(1)(_ + 1)).reduceLeft((x, y) =>
    if (r.nextInt(y._2) == 0) y else x
  )._1
}
