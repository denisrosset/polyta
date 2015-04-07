package com.faacets
package polyta
package formats
package sympol

import spire.math.Rational

import qalg.algebra._

case class ExtData[M, V](
  polyhedron: VPolyhedron[M, V, Rational],
  symmetryInfo: Option[SymmetryInfo])

object ExtData {
  implicit def FormatRead[M, V](implicit M: MatVecInField[M, V, Rational]): FormatRead[ExtData[M, V]] = new ExtDataRead[M, V]
}
