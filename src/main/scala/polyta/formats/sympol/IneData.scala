package com.faacets
package polyta
package formats
package sympol

import spire.math.Rational

import qalg.algebra._

case class IneData[M, V](
  polyhedron: HPolyhedronM[M, V, Rational],
  equalityRows: Set[Int],
  symmetryInfo: Option[SymmetryInfo] = None)

object IneData {
  def fromPolyhedron[M, V](polyhedron: HPolyhedronM[M, V, Rational]): IneData[M, V] = {
    val equalityRows = (polyhedron.inequalities.size until (polyhedron.inequalities.size + polyhedron.equalities.size)).toSet
    IneData(polyhedron, equalityRows)
  }
  implicit def FormatRead[M, V](implicit M: MatVecInField[M, V, Rational]): FormatRead[IneData[M, V]] = new IneDataRead[M, V]
  implicit def FormatWrite[M, V](implicit M: MatVecInField[M, V, Rational]): FormatWrite[IneData[M, V]] = new IneDataWrite[M, V]
}
