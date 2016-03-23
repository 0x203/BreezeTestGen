package de.hpi.asg.breezetestgen.constraintsolving

sealed trait ArithOperator
case object Plus extends ArithOperator
case object Minus extends ArithOperator
case object And extends ArithOperator
case object Or extends ArithOperator
