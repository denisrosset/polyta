package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import spire.math.Rational
import spire.syntax.cfor._
import spire.syntax.action._
import spire.syntax.vectorSpace._
import spire.util._

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

import net.alasc.algebra._
import net.alasc.math.{Perm, Grp}
import net.alasc.std.unit._

/** Does not support unbounded polytopes. */
trait HPolytopeFromV[V, @sp(Double, Long) A] extends HPolytope[V, A] {
  def vPolytope: VPolytopeCombSym[V, A]
  def facetVertexIndices: Seq[Set[Int]]
  def facets = facetVertexIndices.map(new Facet(_))
  def nX = vPolytope.nX
  type G = (Perm, Perm)
  def symGroup = vPolytope.symGroup
  def symSubgroup(f: Facet): Grp[G] = symGroup.setwiseStabilizer(f.vertexIndices, vPolytope.representation)
  object action extends Action[Facet, G] {
    def actr(f: Facet, g: G): Facet = new Facet(f.vertexIndices.map(_ <|+| g._1))
    def actl(g: G, f: Facet): Facet = new Facet(f.vertexIndices.map(g._1 |+|> _))
  }
  require(vPolytope.rays.isEmpty)

  final class Facet(val vertexIndices: Set[Int]) extends FacetBase[V, A] {
    type F = Facet
    def inequality = vPolytope.facetOn(vertexIndices)
    def representatives = new Iterable[Facet] {
      val subgrp = vPolytope.symGroup.setwiseStabilizer(vertexIndices, vPolytope.representation)
      override def size = (vPolytope.symGroup.order / subgrp.order).toInt
      def iterator = {
        val cosets = subgrp \ vPolytope.symGroup
        cosets.iterator.map { coset => new Facet(vertexIndices.map(_ <|+| coset.g._1)) }
      }
    }
  }
}

object HPolytopeFromV {
  final class Impl[V, @sp(Double, Long) A](val vPolytope: VPolytopeCombSym[V, A], val facetVertexIndices: Seq[Set[Int]], val equalities: Seq[LinearEquality[V, A]])(implicit val pack: PackField.ForV[V, A]) extends HPolytopeFromV[V, A] {

  }
  def apply[V, @sp(Double, Long) A](vPolytope: VPolytopeCombSym[V, A], facetVertexIndices: Seq[Set[Int]], equalities: Seq[LinearEquality[V, A]])(implicit pack: PackField.ForV[V, A]) : HPolytopeFromV[V, A] = new Impl(vPolytope, facetVertexIndices, equalities)
}
