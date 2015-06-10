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

trait HAdjacencyFamily[G, V, @sp(Double) A] {
  def hPolytope: SymHPolytope[G, V, A]
  def representativeIndexSet: Set[Int]
  def representative: V
  def representativeIndexSets: Seq[Set[Int]]
  def representatives: Seq[V]
}

object HAdjacencyFamily {
  def apply[G, V, @sp(Double) A: Order, M](hPolytope: SymHPolytope[G, V, A], representativeIndexSet: Set[Int])(implicit alg: AlgMVF[M, V, A]): HAdjacencyFamily[G, V, A] = new HAdjacencyFamilyImpl(hPolytope, representativeIndexSet)
}

final class HAdjacencyFamilyImpl[G, V, @sp(Double) A: Order, M](val hPolytope: SymHPolytope[G, V, A], val representativeIndexSet: Set[Int])(implicit alg: AlgMVF[M, V, A]) extends HAdjacencyFamily[G, V, A] {
  implicit def A: Field[A] = alg.V.A

  implicit val enumerable: EnumerableOrdered[Set[Int], Boolean] = EnumerableOrdered.setInt[Set[Int]](hPolytope.facets.size)
  implicit def G: FiniteGroup[G] = hPolytope.symmetryGroup.algebra
  implicit val permutable: Permutable[Set[Int], G] = Permutable.setInt(hPolytope.facetRepresentation)

  def representative = hPolytope.vertexOn(representativeIndexSet)
  def representativeIndexSets = Representatives.ordered(representativeIndexSet, hPolytope.symmetryGroup).map( (x: RepresentativeOrdered[Set[Int], G]) => x.get).toIndexedSeq
  def representatives = representativeIndexSets.map(hPolytope.vertexOn(_))
}
