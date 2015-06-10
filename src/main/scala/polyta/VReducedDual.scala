package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.all._

import qalg.algebra._
import qalg.algos._

trait VReducedDual[G, V, @sp(Double) A] {
  def vPolytope: SymVPolytope[G, V, A]
  def hReducedPolytope: HPolytope[V, A]
}

object VReducedDual {
  def apply[G, V, @sp(Double) A](vPolytope: SymVPolytope[G, V, A], hReducedPolytope: HPolytope[V, A]): VReducedDual[G, V, A] = new VReducedDualImpl(vPolytope, hReducedPolytope)
}

final class VReducedDualImpl[G, V, @sp(Double) A](val vPolytope: SymVPolytope[G, V, A], val hReducedPolytope: HPolytope[V, A]) extends VReducedDual[G, V, A]
