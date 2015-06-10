package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._

import net.alasc.algebra._
import net.alasc.math.{Perm, Grp}
import net.alasc.math.enum._
import net.alasc.std.set._

import qalg.algebra._
import qalg.algos._

trait VAdjacencyFamilies[G, V, @sp(Double) A] {
  def vPolytope: SymVPolytope[G, V, A]
  def representativeIndexSets: Seq[Set[Int]]
  def representatives: Seq[VAdjacencyFamily[G, V, A]]
}

object VAdjacencyFamilies {
  def apply[G, V, @sp(Double) A: Order](vPolytope: SymVPolytope[G, V, A], representativeIndexSets: Seq[Set[Int]])(implicit alg: AlgVF[V, A]): VAdjacencyFamilies[G, V, A] = new VAdjacencyFamiliesImpl(vPolytope, representativeIndexSets)
}

final class VAdjacencyFamiliesImpl[G, V, @sp(Double) A: Order](val vPolytope: SymVPolytope[G, V, A], val representativeIndexSets: Seq[Set[Int]])(implicit alg: AlgVF[V, A]) extends VAdjacencyFamilies[G, V, A] {
  def representatives: Seq[VAdjacencyFamily[G, V, A]] = representativeIndexSets.map( set => VAdjacencyFamily(vPolytope, set) )
}
