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
}

