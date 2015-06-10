package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.all._

import qalg.algebra._
import qalg.algos._

trait VAdjacencies[V, @sp(Double, Long) A] {
  def vPolytope: VPolytope[V, A]
  def facetSets: Seq[Set[Int]]
  def seq: Seq[VAdjacency[V, A]]
}

final class VAdjacenciesImpl[V, @sp(Double, Long) A: Order](val vPolytope: VPolytope[V, A], val facetSets: Seq[Set[Int]])(implicit alg: AlgVF[V, A]) extends VAdjacencies[V, A] {
  def seq: Seq[VAdjacency[V, A]] = facetSets.map(zs => VAdjacency(vPolytope, zs))
}

object VAdjacencies {
  def apply[V, @sp(Double) A: Order](vPolytope: VPolytope[V, A], facetSets: Seq[Set[Int]])(implicit alg: AlgVF[V, A]): VAdjacencies[V, A] = new VAdjacenciesImpl[V, A](vPolytope, facetSets)
  def apply[V, @sp(Double) A: Order](vPolytope: VPolytope[V, A], hPolytope: HPolytope[V, A])(implicit alg: AlgVF[V, A]): VAdjacencies[V, A] = apply(vPolytope, vPolytope.facetSets(hPolytope))
}
