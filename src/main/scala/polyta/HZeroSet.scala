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

trait HZeroSet[M, V, @sp(Double) A] {
  implicit def alg: AlgMVF[M, V, A]
  implicit def A: Field[A] = alg.M.A
  implicit def orderA: Order[A]

  def vPolyhedron: VPolyhedron[V, A]
  def zeroSet: Set[Int]

  def inequality: LinearInequality[V, A] = {
    val zeroSeq = zeroSet.toSeq
    val zeroH = vPolyhedron.vertices(zeroSeq.head)
    val zeroT = zeroSeq.tail
    val zeroVertices = zeroT.toSeq.map(vPolyhedron.vertices(_) - zeroH)
    val nonZeroIndex = (vPolyhedron.vertices.indices.toSet -- zeroSet).head
    val nonZeroVertex = vPolyhedron.vertices(nonZeroIndex) - zeroH
    val space = MatBuilder[M, A].fromRows(vPolyhedron.nX, zeroVertices :+ nonZeroVertex: _*)
    val gram = space.gramSchmidt
    val lhs = gram(zeroSet.size - 1, ::)
    val rhs = lhs.dot(zeroH)
    if (lhs.dot(nonZeroVertex) < rhs)
      LinearInequalityLE(lhs, rhs)
    else
      LinearInequalityLE(-lhs, -rhs)
  }
}

object HZeroSet {
  protected def build[M, V, @sp(Double) A](vPolyhedron0: VPolyhedron[V, A], zeroSet0: Set[Int])(implicit alg0: AlgMVF[M, V, A], orderA0: Order[A]): HZeroSet[M, V, A] =
    new HZeroSet[M, V, A] {
      def alg = alg0
      def orderA = orderA0
      def vPolyhedron = vPolyhedron0
      def zeroSet = zeroSet0
    }

  def apply[M, V, @sp(Double) A: Order](vPolyhedron: VPolyhedron[V, A], zeroSet: Set[Int])(implicit alg: AlgMVF[M, V, A]): HZeroSet[M, V, A] =
    build(vPolyhedron, zeroSet)
}
