package com.faacets
package polyta
package formats
package qsopt

case class Constraint[A](name: Option[String],
  lhs: Map[String, A], op: ComparisonOperator, rhs: Map[String, A]) {
  def variables: Set[String] = lhs.keys.toSet ++ rhs.keys
}
