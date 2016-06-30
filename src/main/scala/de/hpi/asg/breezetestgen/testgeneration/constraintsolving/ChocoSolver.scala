package de.hpi.asg.breezetestgen.testgeneration.constraintsolving

import de.hpi.asg.breezetestgen.domain.Constant
import de.hpi.asg.breezetestgen.testgeneration.TestBuilder.VariableFixator
import org.chocosolver.solver.Solver
import org.chocosolver.solver.search.loop.monitors.SearchMonitorFactory
import org.chocosolver.solver.variables.{VariableFactory => VF}
import org.chocosolver.solver.constraints.{IntConstraintFactory => ICF}
import org.chocosolver.solver.constraints.{LogicalConstraintFactory => LCF}

object ChocoSolver {
  type V = org.chocosolver.solver.variables.IntVar
  private type BV = org.chocosolver.solver.variables.BoolVar
  private type C = org.chocosolver.solver.constraints.Constraint

  private def instantiateSolver(cc: ConstraintCollection): (Solver, Map[Variable, V]) = {
    val solver = new Solver()

    val boolVariables: Map[BoolVariable, BV] = cc.allVariables.collect{
      case bv: BoolVariable => bv -> VF.bool(bv.name, solver)
    }.toMap

    val variables: Map[Variable, V] = boolVariables ++  cc.allVariables.filterNot(_.isInstanceOf[BoolVariable]).map{
      case v: Variable => v -> VF.enumerated(v.name, v.minValue, v.maxValue, solver)
    }.toMap

    val variableBitArrays = collection.immutable.Map.empty[Variable, Array[BV]]
    val variableBitArray = (v: Variable) => variableBitArrays.get(v) match {
      case Some(a) => a
      case None =>
        val bits = VF.boolArray(s"${v.name}_bits", v.bitCount, solver)
        solver.post(ICF.bit_channeling(bits, variables(v)))
        bits
    }

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
            //we need to model the operation with other constructs, as sum over array or such
            val c: C = tc.op match {
              case Plus => ICF.sum(Array(aVar, bVar), "=", resultVar)
              case Minus => ICF.sum(Array(aVar, VF.minus(bVar)), "=", resultVar)
              case And => ICF.minimum(resultVar, aVar, bVar) //TODO: implement me correctly
              case Or => ICF.maximum(resultVar, aVar, bVar) //TODO: implement me correctly
            }
            postOrReify(c, tc.reifyWith)
          case Right(i) =>
            // with constants we can use the operator directly
            val c = ICF.arithm(resultVar, "=", aVar, tc.operator, i)
            postOrReify(c, tc.reifyWith)
        }
      case BitwiseNot(a, b, rO) =>
        for ((bitA, bitB) <- variableBitArray(a).zip(variableBitArray(b))) {
          postOrReify(ICF.arithm(bitA, "!=", bitB), rO)
        }
      case SelectBits(source, from, to, target, rO) =>
        // source[from:to] should equal target
        val sourceA = variableBitArray(source).drop(from)
        val targetA = variableBitArray(target).take(to - from + 1)
        for ((bitA, bitB) <- targetA.zip(sourceA)) {
          postOrReify(ICF.arithm(bitA, "=", bitB), rO)
        }
      case Combine(a, Right(b: Variable), aIsLeastSignificant, together, rO) =>
        val as = variableBitArray(a)
        val bs = variableBitArray(b)
        val ts = variableBitArray(together)
        val concatenated = if (aIsLeastSignificant) as ++ bs else bs ++ as
        for ((bitA, bitB) <- ts.zip(concatenated)) {
          postOrReify(ICF.arithm(bitA, "=", bitB), rO)
        }
      case Combine(a, Left(b: Constant), aIsLeastSignificant, together, rO) =>
        val as = variableBitArray(a)
        val bs = b.asBitArray.reverse
        val ts = variableBitArray(together)

        // need to separate between this cases, because although arithm is overloaded with boolvar and int types,
        // concatenation of Seq[BoolVar] and Seq[Int] won't work without loosing type information
        if (aIsLeastSignificant) {
          for ((bitA, bitT) <- as.zip(ts)) {
            postOrReify(ICF.arithm(bitA, "=", bitT), rO)
          }
          for ((bitB, bitT) <- bs.zip(ts.drop(as.length))) {
            postOrReify(ICF.arithm(bitT, "=", bitB), rO)
          }
        } else {
          for ((bitB, bitT) <- bs.zip(ts)) {
            postOrReify(ICF.arithm(bitT, "=", bitB), rO)
          }
          for ((bitA, bitT) <- as.zip(ts.drop(bs.length))) {
            postOrReify(ICF.arithm(bitT, "=", bitA), rO)
          }
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
