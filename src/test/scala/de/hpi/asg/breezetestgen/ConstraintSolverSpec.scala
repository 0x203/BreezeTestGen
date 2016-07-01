package de.hpi.asg.breezetestgen

import de.hpi.asg.breezetestgen.domain.Constant
import de.hpi.asg.breezetestgen.testgeneration.VariableData
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

  "Adapt" should "work slicing down" in {
    val bits3 = new VariableData(Variable("bits3", 3, isSigned = false))
    val just2bits = bits3.adapt(2, targetSigned = false)

    val cc = just2bits.addToConstraintCollection(bits3.addToConstraintCollection(ConstraintCollection()))
    val solutions = new ChocoSolver(cc)
    for (solution <- solutions) {
      val a = solution(bits3.underlying).value
      val b = solution(just2bits.underlying).value
      assert(a % 4 == b)
    }
  }

  it should "work adding up" in {
    val bits3 = new VariableData(Variable("bits3", 3, isSigned = false))
    val even6bits = bits3.adapt(6, targetSigned = false)

    val cc = even6bits.addToConstraintCollection(bits3.addToConstraintCollection(ConstraintCollection()))
    val solutions = new ChocoSolver(cc)
    for (solution <- solutions) {
      val a = solution(bits3.underlying).value
      val b = solution(even6bits.underlying).value
      assert(a == b)
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

  it should "work again" in {
    val till16 = Variable("till16", 4, isSigned = false)
    val till16DV = new VariableData(till16)
    val till4DV = till16DV.selectBits(1 to 2)

    val cc = till4DV.addToConstraintCollection(ConstraintCollection().addVariable(till16))
    val solutions = new ChocoSolver(cc)
    for(solution <- solutions) {
      val t16v = solution(till16)
      val t4v = solution(till4DV.underlying)
      assert((t16v.v >> 1) % 4 == t4v.v)
    }
  }

  "Combine" should "work using a constant" in {
    val const2 = Constant(3, 3, isSigned = false)
    val bits3 = Variable("bits3", 3, isSigned = false)
    val combined = Variable("combined", 6, isSigned = false)
    val constraint = Combine(bits3, Left(const2), aIsLeastSignificant = false, combined, None)

    val cc = ConstraintCollection().addVariable(bits3).addVariable(combined).addConstraint(constraint)
    val solutions = new ChocoSolver(cc)
    for (solution <- solutions) {
      val a = solution(bits3).value
      val b = solution(combined).value
      assert(b % 8 == 3)    // constant is ls part
      assert((b >> 3) == a) // upper part is like bits3
    }
  }

  it should "work with single bit, too" in {
    val bit = Variable("bit", 1, isSigned = false)
    val byte = Variable("byte", 8, isSigned = false)
    val combined = Variable("combined", 9, isSigned = false)
    val constraint = Combine(bit, Right(byte), aIsLeastSignificant = true, combined, None)

    val cc = ConstraintCollection().addVariable(byte).addVariable(bit).addVariable(combined).addConstraint(constraint)
    val solutions = new ChocoSolver(cc)
    for (solution <- solutions) {
      val bitV = solution(bit).value
      val byteV = solution(byte).value
      val combinedV = solution(combined).value
      assert(combinedV % 2 == bitV)    // bit is ls part
      assert((combinedV >> 1) == byteV) // upper part is like bits3
    }
  }
}
