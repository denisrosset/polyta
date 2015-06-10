package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import spire.math.Rational
import spire.syntax.cfor._
import spire.syntax.order._
import spire.syntax.innerProductSpace._
import spire.util._

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

import net.alasc.algebra._
import net.alasc.math.{Perm, Grp}

trait VAdjacency[V, @sp(Double) A] {
  def vPolytope: VPolytope[V, A]
  def vertexIndices: Set[Int]
  def facet: LinearInequality[V, A]
}

final class VAdjacencyImpl[V, @sp(Double) A: Order](val vPolytope: VPolytope[V, A], val vertexIndices: Set[Int])(implicit alg: AlgVF[V, A]) extends VAdjacency[V, A] {
  implicit def A: Field[A] = alg.V.A

  override def toString: String = vertexIndices.toSeq.sorted.mkString("HAdjacency(", ", ", ")")

  def facet: LinearInequality[V, A] = vPolytope.facetOn(vertexIndices)
}

object VAdjacency {
  def apply[V, @sp(Double) A: Order](vPolytope: VPolytope[V, A], zeroSet: Set[Int])(implicit alg: AlgVF[V, A]): VAdjacency[V, A] = new VAdjacencyImpl(vPolytope, zeroSet)
}
