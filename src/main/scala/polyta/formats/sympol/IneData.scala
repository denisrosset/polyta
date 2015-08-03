package com.faacets
package polyta
package formats
package sympol

import spire.math.Rational

import qalg.algebra._
import qalg.algos._

import net.alasc.math.Perm
import net.alasc.syntax.all._

case class IneData[V](
  polyhedron: HPolytope[V, Rational],
  equalityRows: Set[Int],
  symmetryInfo: Option[SymmetryInfo] = None)
object IneData {
  def fromPolytope[V](polytope: HPolytope[V, Rational]): IneData[V] = {
    val equalityRows = (polytope.facets.size until (polytope.facets.size + polytope.equalities.size)).toSet
    IneData(polytope, equalityRows)
  }
/*
  def fromSymPolytope[G, V](polytope: SymHPolytope[G, V, Rational]): IneData[V] = {
    val equalityRows = (polytope.facets.size until (polytope.facets.size + polytope.equalities.size)).toSet
    val gens = polytope.symmetryGroup.generators.map(g => Perm.fromImages(polytope.facetRepresentation.images(g))).toSeq
    val order = polytope.symmetryGroup.order
    val symInfo = SymmetryInfo(false, Some(order), gens, Seq.empty[Int])
    IneData(polytope, equalityRows, Some(symInfo))
  }*/

  implicit def FormatRead[V](implicit pack: PackField.ForV[V, Rational]): FormatRead[IneData[V]] = new IneDataRead[V]
  implicit def FormatWrite[V](implicit pack: PackField.ForV[V, Rational]): FormatWrite[IneData[V]] = new IneDataWrite[V]
}
