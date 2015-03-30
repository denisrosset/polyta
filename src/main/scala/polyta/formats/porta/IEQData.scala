package com.faacets
package polyta
package formats
package porta

import spire.math.Rational

case class Constraint[V](lhs: V, op: ComparisonOperator, rhs: Rational)

case class IEQData[M, V](
  dim: Int,
  constraints: Seq[Constraint[V]],
  validPoint: Option[V] = None,
  eliminationOrder: Option[Seq[Int]] = None,
  lowerBounds: Option[V] = None,
  upperBounds: Option[V] = None
)
