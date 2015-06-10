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

trait VAdjacencyFamily[G, V, @sp(Double) A] {
  override def toString = s"Family of size ${representatives.size}, representative $representativeIndexSet"

  def vPolytope: SymVPolytope[G, V, A]
  def representativeIndexSet: Set[Int]
  def representative: LinearInequality[V, A]
  def representativeIndexSets: Seq[Set[Int]]
  def representatives: Seq[LinearInequality[V, A]]
}

object VAdjacencyFamily {
  def apply[G, V, @sp(Double) A: Order](vPolytope: SymVPolytope[G, V, A], representativeIndexSet: Set[Int])(implicit alg: AlgVF[V, A]): VAdjacencyFamily[G, V, A] = new VAdjacencyFamilyImpl(vPolytope, representativeIndexSet)
}

final class VAdjacencyFamilyImpl[G, V, @sp(Double) A: Order](val vPolytope: SymVPolytope[G, V, A], val representativeIndexSet: Set[Int])(implicit alg: AlgVF[V, A]) extends VAdjacencyFamily[G, V, A] {
  implicit def A: Field[A] = alg.V.A

  implicit val enumerable: EnumerableOrdered[Set[Int], Boolean] = EnumerableOrdered.setInt[Set[Int]](vPolytope.vertices.size)
  implicit def G: FiniteGroup[G] = vPolytope.symmetryGroup.algebra
  implicit val permutable: Permutable[Set[Int], G] = Permutable.setInt(vPolytope.vertexRepresentation)

  def representative = vPolytope.facetOn(representativeIndexSet)
  def representativeIndexSets = Representatives.ordered(representativeIndexSet, vPolytope.symmetryGroup).map( (x: RepresentativeOrdered[Set[Int], G]) => x.get).toIndexedSeq
  def representatives = representativeIndexSets.map(vPolytope.facetOn(_))
}
