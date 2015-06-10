package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.all._

import qalg.algebra._
import qalg.algos._

trait HReducedDual[G, V, @sp(Double) A] {
  override def toString: String = s"Polytope $hPolytope with reduced dual $vReducedPolytope"
  def hPolytope: SymHPolytope[G, V, A]
  def vReducedPolytope: VPolytope[V, A]
  def adjacencies: HAdjacencyFamilies[G, V, A]
}

object HReducedDual {
  def apply[G, V, @sp(Double) A: Order, M](hPolytope: SymHPolytope[G, V, A], vReducedPolytope: VPolytope[V, A])(implicit alg: AlgMVF[M, V, A]): HReducedDual[G, V, A] = new HReducedDualImpl(hPolytope, vReducedPolytope)
}

final class HReducedDualImpl[G, M, V, @sp(Double) A: Order](val hPolytope: SymHPolytope[G, V, A], val vReducedPolytope: VPolytope[V, A])(implicit alg: AlgMVF[M, V, A]) extends HReducedDual[G, V, A] {
  def adjacencies = HAdjacencyFamilies(hPolytope, hPolytope.vertexSets(vReducedPolytope))
}

