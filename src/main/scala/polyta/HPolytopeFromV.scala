package com.faacets
package polyta

import spire.algebra._
import spire.std.tuples._
import spire.syntax.action._

import net.alasc.prep._
import net.alasc.prep.PGrp.default._
import net.alasc.perms.Perm
import net.alasc.finite.Grp

import PermPerm._

/** Does not support unbounded polytopes.
  * @param facetVertexIndices Indices of the vertices that support representatives of the facets of the polytope
  * in the H representation. For each family of facets, a single representative is
  * present. 
  */
final class HPolytopeFromV[A](val vPolytope: VPolytopeM[A], val facetVertexIndices: Seq[Set[Int]], val equalities: Seq[LinearEquality[A]])(implicit val A: LinAlg[A]) extends HPolytope[A] {

  require(vPolytope.rays.isEmpty)

  def dim = vPolytope.dim

  type G = (Perm, Perm)

  def facets = facetVertexIndices.map(new Facet(_))

  def allFacets = facets.flatMap(_.representatives)

  def symGroup = vPolytope.symGroup

  final class Facet(val vertexIndices: Set[Int]) extends HPolytope.Facet[A] {
    type F = Facet
    type G = (Perm, Perm)
    def inequality = {
      val onVertices = vertexIndices.toSeq.map(i => vPolytope.allVertices(i))
      val satisfying = vPolytope.allVertices((0 until vPolytope.allVertices.size).find(!vertexIndices.contains(_)).get)
      vPolytope.facetOnVertices(onVertices, satisfying)
    }
    def representatives = new Iterable[Facet] {
      val subgrp = vPolytope.symGroup.in(vPolytope.representation).setwiseStabilizer(vertexIndices)
      override def size = (vPolytope.symGroup.order / subgrp.order).toInt
      def iterator = {
        val cosets = vPolytope.symGroup.rightCosetsBy(subgrp.asSubgroupOf(vPolytope.symGroup).get)
        cosets.iterator.map { coset => new Facet(vertexIndices.map(_ <|+| coset.g._1)) }
      }
    }
    def symSubgroup: Grp[G] = symGroup.in(vPolytope.representation).setwiseStabilizer(vertexIndices)
  }

  object action extends Action[Facet, (Perm, Perm)] {
    def actr(f: Facet, g: (Perm, Perm)): Facet = new Facet(f.vertexIndices.map(i => i <|+| g._1))
    def actl(g: (Perm, Perm), f: Facet): Facet = new Facet(f.vertexIndices.map(i => g._1 |+|> i))
  }

}

object HPolytopeFromV {/*
  final class Impl[V, @sp(Double, Long) A, G](val vPolytope: VPolytopeCombSym[_, V, A, G], val facetVertexIndices: Seq[Set[Int]], val equalities: Seq[LinearEquality[V, A]]) extends HPolytopeFromV[V, A, G] { }
  def apply[V, @sp(Double, Long) A, G](vPolytope: VPolytopeCombSym[_, V, A, G], facetVertexIndices: Seq[Set[Int]], equalities: Seq[LinearEquality[V, A]]) : HPolytopeFromV[V, A, G] = new Impl(vPolytope, facetVertexIndices, equalities)*/
}
