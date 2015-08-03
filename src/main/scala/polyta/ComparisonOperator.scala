package com.faacets
package polyta

sealed trait ComparisonOperator
sealed trait InequalityOperator extends ComparisonOperator

case object LE extends InequalityOperator {
  override def toString = "<="
}

case object EQ extends ComparisonOperator {
  override def toString = "="
}

case object GE extends InequalityOperator {
  override def toString = ">="
}
