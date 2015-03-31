package com.faacets
package polyta
package formats

case class NamedConstraint[A](name: Option[String],
  lhs: Map[String, A], op: ComparisonOperator, rhs: Map[String, A]) {
  def variables: Set[String] = lhs.keys.toSet ++ rhs.keys
}
