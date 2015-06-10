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

trait HAdjacencyFamilies[G, V, @sp(Double) A] {
  override def toString: String = s"Polytope $hPolytope with ${families.size} vertex families"

  def hPolytope: SymHPolytope[G, V, A]
  def representativeIndexSets: Seq[Set[Int]]
  def families: Seq[HAdjacencyFamily[G, V, A]]
}

object HAdjacencyFamilies {
  def apply[G, V, @sp(Double) A: Order, M](hPolytope: SymHPolytope[G, V, A], representativeIndexSets: Seq[Set[Int]])(implicit alg: AlgMVF[M, V, A]): HAdjacencyFamilies[G, V, A] = new HAdjacencyFamiliesImpl(hPolytope, representativeIndexSets)
}

final class HAdjacencyFamiliesImpl[G, V, @sp(Double) A: Order, M](val hPolytope: SymHPolytope[G, V, A], val representativeIndexSets: Seq[Set[Int]])(implicit alg: AlgMVF[M, V, A]) extends HAdjacencyFamilies[G, V, A] {
  def families: Seq[HAdjacencyFamily[G, V, A]] = representativeIndexSets.map( set => HAdjacencyFamily(hPolytope, set) )
}
