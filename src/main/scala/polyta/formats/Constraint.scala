package com.faacets
package polyta
package formats

import qalg.algebra._

sealed trait Constraint[A]

case class NamedConstraint[A](name: Option[String],
  lhs: Map[String, A], op: ComparisonOperator, rhs: Map[String, A]) extends Constraint[A] {
  def variables: Set[String] = lhs.keys.toSet ++ rhs.keys
}

case class VecConstraint[V, A](lhs: V, op: ComparisonOperator, rhs: A)(implicit val V: VecInField[V, A]) extends Constraint[A]
