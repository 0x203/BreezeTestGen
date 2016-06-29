package de.hpi.asg.breezetestgen

import de.hpi.asg.breezetestgen.testgeneration.constraintsolving._

class ConstraintSolverSpec extends baseclasses.UnitTest {

  /*
  import org.chocosolver.solver.Solver
  import org.chocosolver.solver.constraints.IntConstraintFactory
  import org.chocosolver.solver.trace.Chatterbox
  import org.chocosolver.solver.variables.VariableFactory
  "Choco3" should "handle bitchanneling correctly" in {
    val solver = new Solver()
    val bits2  = VariableFactory.integer("bits2", 0, 3, solver)
    val bits2array = VariableFactory.boolArray("bits2_arr", 2, solver)
    val constraint = IntConstraintFactory.bit_channeling(bits2array, bits2)
    solver.post(constraint)

    //Chatterbox.showSolutions(solver)
    solver.findAllSolutions()
    assert(false)
  }

  it should "handle bitwiseNot correctly" in {
    val solver = new Solver()
    val bits2  = VariableFactory.integer("bits2", 0, 3, solver)
    val bits2array = VariableFactory.boolArray("bits2_arr", 2, solver)
    val notbits2  = VariableFactory.integer("notbits2", 0, 3, solver)
    val notbits2array = VariableFactory.boolArray("notbits2_arr", 2, solver)

    solver.post(IntConstraintFactory.bit_channeling(bits2array, bits2))
    solver.post(IntConstraintFactory.bit_channeling(notbits2array, notbits2))

    solver.post(IntConstraintFactory.arithm(bits2array(0), "!=", notbits2array(0)))
    solver.post(IntConstraintFactory.arithm(bits2array(1), "!=", notbits2array(1)))

    Chatterbox.showSolutions(solver)
    solver.findAllSolutions()
    assert(true)
  }
  */


  "BitwiseNot" should "work" in {
    val bits2  = Variable("bits2", 2, isSigned = false)
    val notBits2 = Variable("notBits2", 2, isSigned = false)
    val constraint = BitwiseNot(bits2, notBits2, None)

    val cc = ConstraintCollection().addVariable(bits2).addVariable(notBits2).addConstraint(constraint)
    val solutions = new ChocoSolver(cc)
    for (solution <- solutions) {
      println(s"a is ${solution(bits2)}, b is ${solution(notBits2)}")
      val a = solution(bits2).value
      val b = solution(notBits2).value
      a match {
        case 0 => assert(b == 3)
        case 1 => assert(b == 2)
        case 2 => assert(b == 1)
        case 3 => assert(b == 0)
        case _ => assert(false)
      }
    }
  }
}
