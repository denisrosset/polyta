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
  implicit def alg: AlgMVF[M, V, A]
  implicit def A: Field[A] = alg.M.A
  implicit def orderA: Order[A]

  def hPolyhedron: HPolyhedron[V, A]
  def zeroSet: Set[Int]

  def vertex: V = {
    val ineqSatisfied: Seq[(V, A)] = zeroSet.toSeq.map {
      i => (hPolyhedron.inequalities(i).lhs, hPolyhedron.inequalities(i).rhs)
    }
    val eqSatisfied: Seq[(V, A)] = hPolyhedron.equalities.map( eq => (eq.lhs, eq.rhs) )
    val satisfied = ineqSatisfied ++ eqSatisfied
    val newA = MatBuilder[M, A].fromRows(hPolyhedron.nX, satisfied.map(_._1): _*)
    val newb = VecBuilder[V, A].build(satisfied.map(_._2): _*)
    newA.lu.solveV(newb)
  }
}

object VZeroSet {
  protected def build[M, V, @sp(Double) A](hPolyhedron0: HPolyhedron[V, A], zeroSet0: Set[Int])(implicit alg0: AlgMVF[M, V, A], orderA0: Order[A]): VZeroSet[M, V, A] =
    new VZeroSet[M, V, A] {
      def alg = alg0
      def orderA = orderA0
      def hPolyhedron = hPolyhedron0
      def zeroSet = zeroSet0
    }

  def apply[M, V, @sp(Double) A: Order](hPolyhedron: HPolyhedron[V, A], zeroSet: Set[Int])(implicit alg: AlgMVF[M, V, A]): VZeroSet[M, V, A] =
    build(hPolyhedron, zeroSet)
}
