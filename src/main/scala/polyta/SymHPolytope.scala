package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import spire.math.Rational
import spire.syntax.cfor._
import spire.syntax.vectorSpace._
import spire.util._

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

import net.alasc.algebra._
import net.alasc.math.{Perm, Grp}

trait SymHPolytope[G, V, @sp(Double) A] extends HPolytope[V, A] {
  def symmetryGroup: Grp[G]
  def facetRepresentation: Representation[G]
  def facetSymmetryGroup(v: Int): Grp[G]
  def facetFamilies: Seq[Seq[LinearInequality[V, A]]]
}

final class SymHPolytopeImpl[G, V, @sp(Double) A](val facets: Seq[LinearInequality[V, A]], val equalities: Seq[LinearEquality[V, A]], val symmetryGroup: Grp[G], val facetRepresentation: Representation[G])(implicit alg: AlgVF[V, A]) extends SymHPolytope[G, V, A] {
  def nX = facets.head.lhs.length

  def facetSymmetryGroup(f: Int): Grp[G] = symmetryGroup.stabilizer(f, facetRepresentation)._1
  def facetFamilies: Seq[Seq[LinearInequality[V, A]]] =
    DomainOrbits.orbits(symmetryGroup, facetRepresentation)
      .map(_.toSeq.sorted).toSeq.sortBy(_.head).map( seq => seq.map(facets(_)) )
}


object SymHPolytope {
  def apply[G, V, A](facets: Seq[LinearInequality[V, A]], equalities: Seq[LinearEquality[V, A]], symmetryGroup: Grp[G], facetRepresentation: Representation[G])(implicit alg: AlgVF[V, A]): SymHPolytope[G, V, A] =
    new SymHPolytopeImpl(facets, equalities, symmetryGroup, facetRepresentation)
}
