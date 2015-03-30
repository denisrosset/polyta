package com.faacets
package polyta
package formats
package porta

import spire.math.Rational

case class IEQData[M, V](
  polyhedron: HPolyhedron[M, V, Rational],
  validPoint: Option[V] = None,
  eliminationOrder: Option[Seq[Int]] = None,
  lowerBounds: Option[V] = None,
  upperBounds: Option[V] = None
)
