package com.faacets

import scala.{specialized => sp}

import spire.algebra._
import spire.math.Rational
import spire.syntax.all._
import spire.util._

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

import net.alasc.algebra._
import net.alasc.math.{Perm, Grp}

package object polyta {
  implicit class VPolytopeOps[V, @sp(Double) A](val vPolytope: VPolytope[V, A])(implicit alg: AlgVF[V, A], orderA: Order[A]) {
    def facetSets(hPolytope: HPolytope[V, A]): Seq[Set[Int]] =
      hPolytope.facets.map { facet =>
        vPolytope.vertices.indices.filter { k =>
          val vertex = vPolytope.vertices(k)
          facet.lhs.dot(vertex) === facet.rhs
        }.toSet
      }
    def facetOn(vertexIndices: Set[Int]): LinearInequality[V, A] = {
      implicit def A: Field[A] = alg.V.A
      val zeroSeq = vertexIndices.toSeq
      val zeroH = vPolytope.vertices(zeroSeq.head)
      val zeroT = zeroSeq.tail
      val zeroVertices = zeroT.toSeq.map(vPolytope.vertices(_) - zeroH)
      val nonZeroIndex = (vPolytope.vertices.indices.toSet -- vertexIndices).head
      val nonZeroVertex = vPolytope.vertices(nonZeroIndex) - zeroH
      val lhs = nonZeroVertex.orthogonalized(zeroVertices: _*)
      val rhs = lhs.dot(zeroH)
      if (lhs.dot(nonZeroVertex) < rhs)
        LinearInequalityLE(lhs, rhs)
      else
        LinearInequalityLE(-lhs, -rhs)
    }
    def equalities: Seq[LinearEquality[V, A]] = {
      val headV = vPolytope.vertices.head
      val otherV = vPolytope.vertices.tail
      val basis = otherV.map(_ - headV).orthogonalComplement(vPolytope.nX)
      basis.map( vec => LinearEquality(vec, vec.dot(headV)) )
    }
  }
  implicit class HPolytopeOps[V, @sp(Double) A, M](val hPolytope: HPolytope[V, A])(implicit alg: AlgMVF[M, V, A], orderA: Order[A]) {
    def vertexSets(vPolytope: VPolytope[V, A]): Seq[Set[Int]] =
      vPolytope.vertices.map { vertex =>
        hPolytope.facets.indices.filter { k =>
          val facet = hPolytope.facets(k)
          facet.lhs.dot(vertex) === facet.rhs
        }.toSet
      }

    def vertexOn(facetIndices: Set[Int]): V = {
      val ineqSatisfied: Seq[(V, A)] = facetIndices.toSeq.map {
        i => (hPolytope.facets(i).lhs, hPolytope.facets(i).rhs)
      }
      val eqSatisfied: Seq[(V, A)] = hPolytope.equalities.map( eq => (eq.lhs, eq.rhs) )
      val satisfied = ineqSatisfied ++ eqSatisfied
      val newA: M = MatBuilder[M, A].fromRows(hPolytope.nX, satisfied.map(_._1): _*)
      val newb: V = VecBuilder[V, A].build(satisfied.map(_._2): _*)
      newA.lu.solveV(newb)
    }
  }
}
