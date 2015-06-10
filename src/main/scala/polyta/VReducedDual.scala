package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.all._

import qalg.algebra._
import qalg.algos._

trait VReducedDual[G, V, @sp(Double) A] {
  override def toString: String = s"Polytope $vPolytope with reduced dual $hReducedPolytope"
  def vPolytope: SymVPolytope[G, V, A]
  def hReducedPolytope: HPolytope[V, A]
  def adjacencies: VAdjacencyFamilies[G, V, A]
}

object VReducedDual {
  def apply[G, V, @sp(Double) A: Order](vPolytope: SymVPolytope[G, V, A], hReducedPolytope: HPolytope[V, A])(implicit alg: AlgVF[V, A]): VReducedDual[G, V, A] = new VReducedDualImpl(vPolytope, hReducedPolytope)
}

final class VReducedDualImpl[G, V, @sp(Double) A: Order](val vPolytope: SymVPolytope[G, V, A], val hReducedPolytope: HPolytope[V, A])(implicit alg: AlgVF[V, A]) extends VReducedDual[G, V, A] {
  def adjacencies = VAdjacencyFamilies(vPolytope, vPolytope.facetSets(hReducedPolytope))
}
