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

trait VZeroSet[M, V, @sp(Double) A] {
  implicit def M: MatVecInField[M, V, A]
  implicit def V: VecInField[V, A] = M.V
  implicit def MM: MatMutable[M, A]
  implicit def VM: VecMutable[V, A]
  implicit def A: Field[A] = M.scalar
  implicit def orderA: Order[A]

  def hPolyhedron: HPolyhedron[V, A]
  def zeroSet: Set[Int]

  def vertex: V = {
    val ineqSatisfied: Seq[(V, A)] = zeroSet.toSeq.map {
      i => (hPolyhedron.inequalities(i).lhs, hPolyhedron.inequalities(i).rhs)
    }
    val eqSatisfied: Seq[(V, A)] = hPolyhedron.equalities.map( eq => (eq.lhs, eq.rhs) )
    val satisfied = ineqSatisfied ++ eqSatisfied
    val newA = M.fromRows(hPolyhedron.nX, satisfied.map(_._1): _*)
    val newb = V.build(satisfied.map(_._2): _*)
    lu(newA).solve(newb)
  }
}

object VZeroSet {
  protected def build[M, V, @sp(Double) A](hPolyhedron0: HPolyhedron[V, A], zeroSet0: Set[Int])(implicit M0: MatVecInField[M, V, A], MM0: MatMutable[M, A], VM0: VecMutable[V, A], orderA0: Order[A]): VZeroSet[M, V, A] =
    new VZeroSet[M, V, A] {
      def M = M0
      def MM = MM0
      def VM = VM0
      def orderA = orderA0
      def hPolyhedron = hPolyhedron0
      def zeroSet = zeroSet0
    }

  def apply[M, V, @sp(Double) A](hPolyhedron: HPolyhedron[V, A], zeroSet: Set[Int])(implicit M: MatVecInField[M, V, A], MM: MatMutable[M, A], VM: VecMutable[V, A], orderA: Order[A]): VZeroSet[M, V, A] =
    build(hPolyhedron, zeroSet)
}
