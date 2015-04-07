package com.faacets
package polyta
package formats
package porta

import spire.math.Rational

import qalg.algebra._

case class IEQData[V](
  dim: Int,
  constraints: Seq[VConstraint[V, Rational]],
  validPoint: Option[V] = None,
  eliminationOrder: Option[Seq[Int]] = None,
  lowerBounds: Option[V] = None,
  upperBounds: Option[V] = None
)

object IEQData {
  implicit def FormatRead[V](implicit V0: VecInField[V, Rational]): FormatRead[IEQData[V]] = new IEQDataRead[V] {
    def V = V0
  }
  implicit def FormatWrite[V](implicit V0: VecInField[V, Rational]): FormatWrite[IEQData[V]] = new IEQDataWrite[V] {
    def V = V0
  }
  implicit def ToHPolyhedron[M, V](implicit M0: MatVecInField[M, V, Rational]): Converter[IEQData[V], HPolyhedron[M, V, Rational]] = new IEQDataToHPolyhedron[M, V] {
    def M = M0
  }
  implicit def FromHPolyhedron[M, V](implicit M0: MatVecInField[M, V, Rational]): Converter[HPolyhedron[M, V, Rational], IEQData[V]] = new IEQDataFromVPolyhedron[M, V] {
    def M = M0
  }
}
