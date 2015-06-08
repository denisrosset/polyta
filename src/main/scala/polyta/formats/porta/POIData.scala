package com.faacets
package polyta
package formats
package porta

import qalg.algebra._
import qalg.algos._
import spire.math.Rational

case class POIData[M, V](polyhedron: VPolyhedronM[M, V, Rational])

object POIData {
  implicit def FormatRead[M, V](implicit alg0: AlgMVF[M, V, Rational]): FormatRead[POIData[M, V]] = new POIDataRead[M, V] {
    def alg = alg0
  }
  implicit def FormatWrite[M, V](implicit alg: AlgMVF[M, V, Rational]): FormatWrite[POIData[M, V]] = new POIDataWrite[M, V]
}
