package com.faacets
package polyta
package formats
package porta

import spire.math.Rational

import qalg.algebra._
import qalg.algos._

case class IEQData[M, V](
  polyhedron: HPolyhedronM[M, V, Rational],
  validPoint: Option[V] = None,
  eliminationOrder: Option[Seq[Int]] = None,
  lowerBounds: Option[V] = None,
  upperBounds: Option[V] = None
)

object IEQData {
  def empty[M, V](dim: Int)(implicit alg: AlgMVF[M, V, Rational]): IEQData[M, V] = IEQData[M, V](HPolyhedronM.empty[M, V, Rational](dim))
  implicit def FormatRead[M, V](implicit alg: AlgMVF[M, V, Rational]): FormatRead[IEQData[M, V]] = new IEQDataRead[M, V]
  implicit def FormatWrite[M, V](implicit alg: AlgMVF[M, V, Rational]): FormatWrite[IEQData[M, V]] = new IEQDataWrite[M, V]
}
