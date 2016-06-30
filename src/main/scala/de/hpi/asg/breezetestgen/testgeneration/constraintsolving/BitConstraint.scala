package de.hpi.asg.breezetestgen.testgeneration.constraintsolving

import de.hpi.asg.breezetestgen.domain.Constant


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

case class Combine(a: Variable,
                   b: Either[Constant, Variable],
                   aIsLeastSignificant: Boolean,
                   together: Variable,
                   override val reifyWith: Option[BoolVariable]) extends Constraint(reifyWith)


//xor(o: Data)
