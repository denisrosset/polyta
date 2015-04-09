package com.faacets
package polyta
package formats
package sympol

import spire.math.Rational

import qalg.algebra._

case class ExtData[M, V](
  polyhedron: VPolyhedron[M, V, Rational],
  rayRows: Set[Int],
  symmetryInfo: Option[SymmetryInfo] = None)

object ExtData {
  def fromPolyhedron[M, V](polyhedron: VPolyhedron[M, V, Rational]): ExtData[M, V] = {
    val rayRows = (polyhedron.nVertices until (polyhedron.nVertices + polyhedron.nRays)).toSet
    ExtData(polyhedron, rayRows)
  }
  implicit def FormatRead[M, V](implicit M: MatVecInField[M, V, Rational]): FormatRead[ExtData[M, V]] = new ExtDataRead[M, V]
  implicit def FormatWrite[M, V](implicit M: MatVecInField[M, V, Rational]): FormatWrite[ExtData[M, V]] = new ExtDataWrite[M, V]
}
