package de.hpi.asg.breezetestgen.testgeneration.constraintsolving


case class BitwiseNot(source: Variable,
                      target:Variable,
                      override val reifyWith: Option[BoolVariable]) extends Constraint(reifyWith)

case class Adapt(source: Variable,
                 target: Variable,
                 override val reifyWith: Option[BoolVariable]) extends Constraint(reifyWith)

case class SelectBits(source: Variable,
                      from: Int,
                      to: Int,
                      target: Variable,
                      override val reifyWith: Option[BoolVariable]) extends Constraint(reifyWith)

//xor(o: Data)
//combineWithMoreSignificant(o: Data)
//combineWitLessSignificant(o: Data)
