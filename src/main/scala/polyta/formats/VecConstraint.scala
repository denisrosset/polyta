package com.faacets
package polyta
package formats

case class VecConstraint[V, A](lhs: V, op: ComparisonOperator, rhs: A)
