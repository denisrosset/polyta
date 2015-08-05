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
  polytope: HPolytope[V, Rational],
  equalityRows: Set[Int],
  symmetryInfo: Option[SymmetryInfo] = None)

object IneData {
  def fromPolytope[V](polytope: HPolytope[V, Rational]): IneData[V] = {
    val equalityRows = (polytope.facets.size until (polytope.facets.size + polytope.equalities.size)).toSet
    IneData(polytope, equalityRows)
  }

  def fromSymPolytope[V, G](polytope: HPolytopeCombSym[_, V, Rational, G]): IneData[V] = {
    val equalityRows = (polytope.facets.size until (polytope.facets.size + polytope.equalities.size)).toSet
    val generators = polytope.symGroup.generators.toSeq
    implicit def action = polytope.facetIndexAction
    val permutations = generators.map(_.to[Perm])
    val order = polytope.symGroup.order
    val symInfo = SymmetryInfo(false, Some(order), permutations, Seq.empty[Int])
    IneData(polytope, equalityRows, Some(symInfo))
  }

  implicit def FormatRead[V](implicit pack: PackField.ForV[V, Rational]): FormatRead[IneData[V]] = new IneDataRead[V]
  implicit def FormatWrite[V](implicit pack: PackField.ForV[V, Rational]): FormatWrite[IneData[V]] = new IneDataWrite[V]
}
