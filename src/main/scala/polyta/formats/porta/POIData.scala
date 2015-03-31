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
  implicit def FormatWrite[M, V](implicit M0: MatVecInField[M, V, Rational]): FormatWrite[POIData[M, V]] = new POIDataWrite[M, V] {
    def M = M0
  }
  implicit def ToVPolyhedron[M, V](implicit M0: MatVecInField[M, V, Rational]): Converter[POIData[M, V], VPolyhedron[M, V, Rational]] = new POIDataToVPolyhedron[M, V] {
    def M = M0
  }
  implicit def FromVPolyhedron[M, V](implicit M0: MatVecInField[M, V, Rational]): Converter[VPolyhedron[M, V, Rational], POIData[M, V]] = new POIDataFromVPolyhedron[M, V] {
    def M = M0
  }
}
