package com.faacets
package polyta
package formats
package sympol

import spire.math.Rational

import qalg.algebra._

case class IneData[V](
  polyhedron: HPolyhedron[V, Rational],
  equalityRows: Set[Int],
  symmetryInfo: Option[SymmetryInfo] = None)

object IneData {
  def fromPolyhedron[V](polyhedron: HPolyhedron[V, Rational]): IneData[V] = {
    val equalityRows = (polyhedron.inequalities.size until (polyhedron.inequalities.size + polyhedron.equalities.size)).toSet
    IneData(polyhedron, equalityRows)
  }
  implicit def FormatRead[V](implicit V: VecInField[V, Rational]): FormatRead[IneData[V]] = new IneDataRead[V]
  implicit def FormatWrite[V](implicit V: VecInField[V, Rational]): FormatWrite[IneData[V]] = new IneDataWrite[V]
}
