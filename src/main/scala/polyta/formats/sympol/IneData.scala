package com.faacets
package polyta
package formats
package sympol

import spire.math.Rational

import qalg.algebra._

case class IneData[M, V](
  polyhedron: HFullPolyhedron[M, V, Rational],
  symmetryInfo: Option[SymmetryInfo])

object IneData {
  implicit def FormatRead[M, V](implicit M: MatVecInField[M, V, Rational]): FormatRead[IneData[M, V]] = new IneDataRead[M, V]
}
