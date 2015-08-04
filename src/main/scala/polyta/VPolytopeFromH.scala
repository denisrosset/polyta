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

/** Polytope in the V representation, obtained from the conversion of a polytope in the
  * H representation.
  * Does not (yet) support unbounded polytopes (i.e. with rays). */
trait VPolytopeFromH[V, @sp(Double, Long) A, G0] extends VPolytope[V, A] {

  /** Polytope in the H representation, with a possible combinatorial symmetry. */
  val hPolytope: HPolytopeCombSym[_, V, A, G0]
  /** Indices of the facets that support representatives of the vertices of the polytope
    * in the V representation. For each family of vertices, a single representative is
    * present. */
  def vertexFacetIndices: Seq[Set[Int]]

  type G = G0

  implicit val pack: PackField.ForV[V, A] = hPolytope.pack
  implicit var orderA: Order[A] = hPolytope.orderA

  val nX = hPolytope.nX

  def vertices = vertexFacetIndices.map(new Vertex(_))
  def rays = Seq.empty
  def symGroup = hPolytope.symGroup

  trait Element extends ElementBase[V, G]

  type Ray = Nothing

  final class Vertex(val facetIndices: Set[Int]) extends Element with VertexBase[V, G0] {
    type E = Vertex
    def point = hPolytope.vertexOn(facetIndices.toSeq.map(hPolytope.allFacets(_)))
    def symSubgroup: Grp[G] = symGroup.setwiseStabilizer(facetIndices, hPolytope.representation)
    def representatives = new Iterable[Vertex] {
      val subgrp = symSubgroup
      override def size = (hPolytope.symGroup.order / subgrp.order).toInt
      def iterator = {
        val cosets = subgrp \ hPolytope.symGroup
        cosets.iterator.map { coset => new Vertex(facetIndices.map(hPolytope.facetIndexAction.actr(_, coset.g))) }
      }
    }
  }

  object elementAction extends Action[Element, G] {
    def actr(e: Element, g: G): Element = e match {
      case v: Vertex => vertexAction.actr(v, g)
      case _ => e
    }
    def actl(g: G, e: Element) = e match {
      case v: Vertex => vertexAction.actl(g, v)
      case _ => e
    }
  }
  object vertexAction extends Action[Vertex, G] {
    def actr(v: Vertex, g: G): Vertex = new Vertex(v.facetIndices.map(hPolytope.facetIndexAction.actr(_, g)))
    def actl(g: G, v: Vertex): Vertex = new Vertex(v.facetIndices.map(hPolytope.facetIndexAction.actl(g, _)))
  }
  object rayAction extends Action[Ray, G] {
    def actr(r: Ray, g: G): Ray = r
    def actl(g: G, r: Ray): Ray = r
  }
}

object VPolytopeFromH {
  final class Impl[V, @sp(Double, Long) A, G](val hPolytope: HPolytopeCombSym[_, V, A, G], val vertexFacetIndices: Seq[Set[Int]]) extends VPolytopeFromH[V, A, G] { }
  def apply[V, @sp(Double, Long) A, G](hPolytope: HPolytopeCombSym[_, V, A, G], vertexFacetIndices: Seq[Set[Int]]): VPolytopeFromH[V, A, G] = new Impl(hPolytope, vertexFacetIndices)
}
