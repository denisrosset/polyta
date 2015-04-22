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
  implicit def M: MatVecInField[M, V, A]
  implicit def V: VecInField[V, A] = M.V
  implicit def MM: MatMutable[M, A]
  implicit def A: Field[A] = M.scalar
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
    val space = M.fromRows(vPolyhedron.nX, zeroVertices :+ nonZeroVertex: _*)
    val gram = gramSchmidt(space)
    val lhs = gram(zeroSet.size - 1, ::)
    val rhs = lhs.dot(zeroH)
    if (lhs.dot(nonZeroVertex) < rhs)
      LinearInequalityLE(lhs, rhs)
    else
      LinearInequalityLE(-lhs, -rhs)
  }
}

object HZeroSet {
  protected def build[M, V, @sp(Double) A](vPolyhedron0: VPolyhedron[V, A], zeroSet0: Set[Int])(implicit M0: MatVecInField[M, V, A], MM0: MatMutable[M, A], orderA0: Order[A]): HZeroSet[M, V, A] =
    new HZeroSet[M, V, A] {
      def M = M0
      def MM = MM0
      def orderA = orderA0
      def vPolyhedron = vPolyhedron0
      def zeroSet = zeroSet0
    }

  def apply[M, V, @sp(Double) A](vPolyhedron: VPolyhedron[V, A], zeroSet: Set[Int])(implicit M: MatVecInField[M, V, A], MM: MatMutable[M, A], orderA: Order[A]): HZeroSet[M, V, A] =
    build(vPolyhedron, zeroSet)
}
