package de.hpi.asg.breezetestgen

import de.hpi.asg.breezetestgen.testgeneration.constraintsolving._

class ConstraintSolverSpec extends baseclasses.UnitTest {

  "BitwiseNot" should "work" in {
    val bits2  = Variable("bits2", 2, isSigned = false)
    val notBits2 = Variable("notBits2", 2, isSigned = false)
    val constraint = BitwiseNot(bits2, notBits2, None)

    val cc = ConstraintCollection().addVariable(bits2).addVariable(notBits2).addConstraint(constraint)
    val solutions = new ChocoSolver(cc)
    for (solution <- solutions) {
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

  "SelectBits" should "work" in {
    val bits3  = Variable("bits3", 3, isSigned = false)
    val selected = Variable("selected", 2, isSigned = false)
    val constraint = SelectBits(bits3, 1, 2, selected, None)

    val cc = ConstraintCollection().addVariable(bits3).addVariable(selected).addConstraint(constraint)
    val solutions = new ChocoSolver(cc)
    for (solution <- solutions) {
      val a = solution(bits3).value
      val b = solution(selected).value
      assert((a >> 1) == b)
    }
  }
}
