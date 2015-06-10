package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.all._

import qalg.algebra._
import qalg.algos._

trait HReducedDual[G, V, @sp(Double) A] {
  def hPolytope: SymHPolytope[G, V, A]
  def vReducedPolytope: VPolytope[V, A]
}

object HReducedDual {
  def apply[G, V, @sp(Double) A](hPolytope: SymHPolytope[G, V, A], vReducedPolytope: VPolytope[V, A]): HReducedDual[G, V, A] = new HReducedDualImpl(hPolytope, vReducedPolytope)
}

final class HReducedDualImpl[G, V, @sp(Double) A](val hPolytope: SymHPolytope[G, V, A], val vReducedPolytope: VPolytope[V, A]) extends HReducedDual[G, V, A]
