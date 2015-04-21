package com.faacets
package polyta
package formats
package sympol

import spire.math.Rational

import qalg.algebra._

case class ExtData[M, V](
  polyhedron: VPolyhedronM[M, V, Rational],
  rayCols: Set[Int],
  symmetryInfo: Option[SymmetryInfo] = None)

object ExtData {
  def fromPolyhedron[M, V](polyhedron: VPolyhedronM[M, V, Rational]): ExtData[M, V] = {
    val rayCols = (polyhedron.vertices.size until (polyhedron.vertices.size + polyhedron.rays.size)).toSet
    ExtData(polyhedron, rayCols)
  }
  implicit def FormatRead[M, V](implicit M: MatVecInField[M, V, Rational]): FormatRead[ExtData[M, V]] = new ExtDataRead[M, V]
  implicit def FormatWrite[M, V](implicit M: MatVecInField[M, V, Rational]): FormatWrite[ExtData[M, V]] = new ExtDataWrite[M, V]
}
