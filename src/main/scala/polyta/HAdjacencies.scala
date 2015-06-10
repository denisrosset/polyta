package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.all._

import qalg.algebra._
import qalg.algos._

trait HAdjacencies[V, @sp(Double, Long) A] {
  def hPolytope: HPolytope[V, A]
  def vertexSets: Seq[Set[Int]]
  def seq: Seq[HAdjacency[V, A]]
}

object HAdjacencies {
  def apply[V, @sp(Double) A: Order, M](hPolytope: HPolytope[V, A], vertexSets: Seq[Set[Int]])(implicit alg: AlgMVF[M, V, A]): HAdjacencies[V, A] = new HAdjacenciesImpl[M, V, A](hPolytope, vertexSets)
  def apply[V, @sp(Double) A: Order, M](hPolytope: HPolytope[V, A], vPolytope: VPolytope[V, A])(implicit alg: AlgMVF[M, V, A]): HAdjacencies[V, A] = {
    val zeroSets = vPolytope.vertices.map { vertex =>
      hPolytope.facets.indices.filter { k =>
        val facet = hPolytope.facets(k)
        facet.lhs.dot(vertex) === facet.rhs
      }.toSet
    }
    apply(hPolytope, zeroSets)
  }
}

final class HAdjacenciesImpl[M, V, @sp(Double, Long) A: Order](val hPolytope: HPolytope[V, A], val vertexSets: Seq[Set[Int]])(implicit alg: AlgMVF[M, V, A]) extends HAdjacencies[V, A] {
  def seq: Seq[HAdjacency[V, A]] = vertexSets.map(zs => HAdjacency(hPolytope, zs))
}
