package de.hpi.asg.breezetestgen.constraintsolving

import de.hpi.asg.breezetestgen.domain.Constant
import de.hpi.asg.breezetestgen.testgeneration.TestBuilder.VariableFixator
import org.chocosolver.solver.Solver
import org.chocosolver.solver.search.loop.monitors.SearchMonitorFactory
import org.chocosolver.solver.variables.{VariableFactory => VF}
import org.chocosolver.solver.constraints.{IntConstraintFactory => ICF}

object ChocoSolver {
  type V = org.chocosolver.solver.variables.IntVar
  private type BV = org.chocosolver.solver.variables.BoolVar
  private type C = org.chocosolver.solver.constraints.Constraint

  private def instantiateSolver(cc: ConstraintCollection): (Solver, Map[Variable, V]) = {
    val solver = new Solver()

    val boolVariables: Map[BoolVariable, BV] = cc.allVariables.collect{
      case bv: BoolVariable => bv -> VF.bool(bv.name, solver)
    }.toMap

    val variables: Map[Variable, V] = cc.allVariables.filterNot(_.isInstanceOf[BoolVariable]).map{
      case v: Variable => v -> VF.enumerated(v.name, v.minValue, v.maxValue, solver)
    }.toMap ++ boolVariables

    val postOrReify = (c: C,r: Option[BoolVariable]) => r match {
      case Some(v) => c.reifyWith(boolVariables(v))
      case None => solver.post(c)
    }

    cc.allConstraints.foreach{
      case bc: BinaryConstraint =>
        //println(s"BinaryConstraint: ${bc.a.name} ${bc.operator} ${bc.b}; reify: ${bc.reifyWith}")
        val c: C = bc.b match {
          case Left(v) => ICF.arithm(variables(bc.a), bc.operator, variables(v))
          case Right(i) => ICF.arithm(variables(bc.a), bc.operator, i)
        }
        postOrReify(c, bc.reifyWith)

      case tc: TernaryConstraint =>
        val resultVar = variables(tc.result)
        val aVar = variables(tc.a)

        tc.b match {
          case Left(v) =>
            val bVar = variables(v)
            val c: C = tc.op match {
              case Plus => ICF.sum(Array(aVar, bVar), "=", resultVar)
              case Minus => ICF.sum(Array(aVar, VF.minus(bVar)), "=", resultVar)
              case And => ICF.minimum(resultVar, aVar, bVar) //TODO: implement me correctly
              case Or => ICF.maximum(resultVar, aVar, bVar) //TODO: implement me correctly
            }
            postOrReify(c, tc.reifyWith)
          case Right(i) =>
            val c = ICF.arithm(resultVar, "=", aVar, tc.operator, i)
            postOrReify(c, tc.reifyWith)
        }
    }
    (solver, variables)
  }
}

class ChocoSolver(cc: ConstraintCollection) extends Iterator[VariableFixator]{
  import ChocoSolver._

  private val (solver, variables) = instantiateSolver(cc)

  var hasNext = solver.findSolution()

  val isFeasible = hasNext

  def next = {
    val vf: Map[Variable, Constant] = variables.map{case (k, v) => k -> Constant(v.getValue, k.bitCount, k.isSigned)}
    hasNext = solver.nextSolution()
    vf
  }

  def printSolutions(vars: Seq[Variable], limitCount: Int = 5) = {
    println(s"Looking for solutions:")
    SearchMonitorFactory.limitSolution(solver, limitCount)

    take(limitCount).foreach{case f =>
      println(vars.map(v => s"${v.name}: ${f(v)}").mkString(", "))
    }
  }
}
