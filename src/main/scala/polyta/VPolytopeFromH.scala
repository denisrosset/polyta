package com.faacets
package polyta

import spire.algebra._
import spire.math.Rational
import spire.syntax.cfor._
import spire.syntax.action._
import spire.syntax.vectorSpace._
import spire.util._

import scalin.{Mat, Vec}
import scalin.syntax.all._

import net.alasc.algebra._
import net.alasc.perms.Perm
import net.alasc.finite.Grp
import net.alasc.std.unit._

/** Polytope in the V representation, obtained from the conversion of a polytope in the
  * H representation.
  * Does not (yet) support unbounded polytopes (i.e. with rays). 
  * 
  * @param hPolytope          Polytope in the H representation, with a possible combinatorial symmetry. 
  * 
  * @param vertexFacetIndices Indices of the facets that support representatives of the vertices of the polytope
  *                           in the V representation. For each family of vertices, a single representative is
  *                           present.
  */
class VPolytopeFromH[A](val hPolytope: HPolytopeM[A], val vertexFacetIndices: Seq[Set[Int]]) extends VPolytope[A] {
  
  type G = Perm

  val dim = hPolytope.dim

  def vertices = vertexFacetIndices.map(new Vertex(_))
  def rays = Seq.empty
  def symGroup = hPolytope.symGroup

  trait Element extends VPolytope.Element[A]

  type Ray = Nothing

  final class Vertex(val facetIndices: Set[Int]) extends Element with VPolytope.Vertex[A] {
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
