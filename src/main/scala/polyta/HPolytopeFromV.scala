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
trait HPolytopeFromV[V, @sp(Double, Long) A, G0] extends HPolytope[V, A] {

  /** Polytope in the V representation, with a possible combinatorial symmetry. */
  val vPolytope: VPolytopeCombSym[_, V, A, G0]
  /** Indices of the vertices that support representatives of the facets of the polytope
    * in the H representation. For each family of facets, a single representative is
    * present. */
  def facetVertexIndices: Seq[Set[Int]]

  type G = G0

  implicit val pack: PackField.ForV[V, A] = vPolytope.pack
  implicit var orderA: Order[A] = vPolytope.orderA

  val nX = vPolytope.nX

  def facets = facetVertexIndices.map(new Facet(_))
  def symGroup = vPolytope.symGroup
  object action extends Action[Facet, G] {
    def actr(f: Facet, g: G): Facet = new Facet(f.vertexIndices.map(i => vPolytope.vertexIndexAction.actr(i, g)))
    def actl(g: G, f: Facet): Facet = new Facet(f.vertexIndices.map(i => vPolytope.vertexIndexAction.actl(g, i)))
  }
  require(vPolytope.rays.isEmpty)

  final class Facet(val vertexIndices: Set[Int]) extends FacetBase[V, A, G] {
    type F = Facet
    def inequality = {
      val onVertices = vertexIndices.toSeq.map(i => vPolytope.allVertices(i))
      val satisfying = vPolytope.allVertices((0 until vPolytope.allVertices.size).find(!vertexIndices.contains(_)).get)
      vPolytope.facetOn(onVertices, satisfying)
    }
    def representatives = new Iterable[Facet] {
      val subgrp = vPolytope.symGroup.setwiseStabilizer(vertexIndices, vPolytope.representation)
      override def size = (vPolytope.symGroup.order / subgrp.order).toInt
      def iterator = {
        val cosets = subgrp \ vPolytope.symGroup
        cosets.iterator.map { coset => new Facet(vertexIndices.map(vPolytope.vertexIndexAction.actr(_, coset.g))) }
      }
    }
    def symSubgroup: Grp[G] = symGroup.setwiseStabilizer(vertexIndices, vPolytope.representation)
  }
}

object HPolytopeFromV {
  final class Impl[V, @sp(Double, Long) A, G](val vPolytope: VPolytopeCombSym[_, V, A, G], val facetVertexIndices: Seq[Set[Int]], val equalities: Seq[LinearEquality[V, A]]) extends HPolytopeFromV[V, A, G] { }
  def apply[V, @sp(Double, Long) A, G](vPolytope: VPolytopeCombSym[_, V, A, G], facetVertexIndices: Seq[Set[Int]], equalities: Seq[LinearEquality[V, A]]) : HPolytopeFromV[V, A, G] = new Impl(vPolytope, facetVertexIndices, equalities)
}
