package com.faacets
package polyta
package formats
package porta

import qalg.algebra._
import spire.math.Rational

case class POIData[M, V](polyhedron: VPolyhedron[M, V, Rational])

object POIData {
  implicit def FormatRead[M, V](implicit M0: MatVecInField[M, V, Rational]): FormatRead[POIData[M, V]] = new POIDataRead[M, V] {
    def M = M0
  }
  implicit def FormatWrite[M, V](implicit M: MatVecInField[M, V, Rational]): FormatWrite[POIData[M, V]] = new POIDataWrite[M, V]
}
