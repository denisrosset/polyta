package com.faacets
package polyta

sealed trait ComparisonOperator

case object LE extends ComparisonOperator {
  override def toString = "<="
}

case object EQ extends ComparisonOperator {
  override def toString = "="
}

case object GE extends ComparisonOperator {
  override def toString = ">="
}
