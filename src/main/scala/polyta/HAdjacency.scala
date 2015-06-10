package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import spire.math.Rational
import spire.syntax.cfor._
import spire.syntax.eq._
import spire.syntax.innerProductSpace._
import spire.util._

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

import net.alasc.algebra._
import net.alasc.math.{Perm, Grp}

trait HAdjacency[V, @sp(Double) A] {
  def hPolytope: HPolytope[V, A]
  def facetIndices: Set[Int]
  def vertex: V
}

final class HAdjacencyImpl[M, V, @sp(Double) A: Order](val hPolytope: HPolytope[V, A], val facetIndices: Set[Int])(implicit alg: AlgMVF[M, V, A]) extends HAdjacency[V, A] {
  implicit def A: Field[A] = alg.V.A

  override def toString: String = facetIndices.toSeq.sorted.mkString("HAdjacency(", ", ", ")")

  def vertex: V = hPolytope.vertexOn(facetIndices)
}

object HAdjacency {
  def apply[V, @sp(Double) A: Order, M](hPolytope: HPolytope[V, A], facetIndices: Set[Int])(implicit alg: AlgMVF[M, V, A]): HAdjacency[V, A] = new HAdjacencyImpl[M, V, A](hPolytope, facetIndices)
}
