package com.faacets
package polyta
package formats
package sympol

import spire.math.Rational

import qalg.algebra._

case class ExtData[V](
  polyhedron: VPolyhedron[V, Rational],
  rayCols: Set[Int],
  symmetryInfo: Option[SymmetryInfo] = None)

object ExtData {
  def fromPolyhedron[V](polyhedron: VPolyhedron[V, Rational]): ExtData[V] = {
    val rayCols = (polyhedron.vertices.size until (polyhedron.vertices.size + polyhedron.rays.size)).toSet
    ExtData(polyhedron, rayCols)
  }
  implicit def FormatRead[V](implicit V: VecInField[V, Rational]): FormatRead[ExtData[V]] = new ExtDataRead[V]
  implicit def FormatWrite[V](implicit V: VecInField[V, Rational]): FormatWrite[ExtData[V]] = new ExtDataWrite[V]
}
