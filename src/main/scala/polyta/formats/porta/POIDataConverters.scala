package com.faacets
package polyta
package formats
package porta

import qalg.algebra._
import spire.math.Rational

trait POIDataToVPolyhedron[M, V] extends Converter[POIData[M, V], VPolyhedron[M, V, Rational]] {
  implicit def M: MatVecInField[M, V, Rational]

  def convert(from: POIData[M, V]): VPolyhedron[M, V, Rational] = from.polyhedron
}

trait POIDataFromVPolyhedron[M, V] extends Converter[VPolyhedron[M, V, Rational], POIData[M, V]] {
  implicit def M: MatVecInField[M, V, Rational]

  def convert(from: VPolyhedron[M, V, Rational]): POIData[M, V] = POIData(from)
}
