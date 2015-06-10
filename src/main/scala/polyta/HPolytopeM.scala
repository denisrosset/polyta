package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import spire.math.Rational
import spire.syntax.cfor._
import spire.syntax.vectorSpace._
import spire.util._

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

import net.alasc.algebra._
import net.alasc.math.{Perm, Grp}

trait HPolytopeM[M, V, @sp(Double, Long) A] extends HPolytope[V, A] {
  implicit def alg: AlgMVF[M, V, A]
  implicit def A: Field[A] = alg.M.A

  def equalities: IndexedSeq[LinearEquality[V, A]] = new IndexedSeq[LinearEquality[V, A]] {
    def length = mAeq.nRows
    def apply(r: Int): LinearEquality[V, A] = LinearEquality(mAeq(r, ::), vbeq(r))
  }

  def facets: IndexedSeq[LinearInequality[V, A]] = new IndexedSeq[LinearInequality[V, A]] {
    def length = mA.nRows
    def apply(r: Int): LinearInequality[V, A] = LinearInequalityLE(mA(r, ::), vb(r))
  }

  def mA: M
  def vb: V
  def mAeq: M
  def vbeq: V

  def nX: Int = mA.nCols
  require(nX == mAeq.nCols)
}

object HPolytopeM {
/*
  def fromEqualities[M, V, @sp(Double) A](mAeq: M, vbeq: V)(implicit alg: AlgMVF[M, V, A]): HPolytopeM[M, V, A] =
    apply(mA = zeros[M](0, mAeq.nCols), vb = zeros[V](0), mAeq = mAeq, vbeq = vbeq)

  def fromInequalities[M, V, @sp(Double) A](mA: M, vb: V)(implicit alg: AlgMVF[M, V, A]): HPolytopeM[M, V, A] =
    apply(mA = mA, vb = vb, mAeq = zeros[M](0, mA.nCols), vbeq = zeros[V](0))
 */
  def apply[M, V, @sp(Double) A](mA: M, vb: V, mAeq: M, vbeq: V)(implicit alg: AlgMVF[M, V, A]): HPolytopeM[M, V, A] = new HPolytopeMImpl(mA, vb, mAeq, vbeq)
/*
  def empty[M, V, @sp(Double) A](d: Int)(implicit alg: AlgMVF[M, V, A]): HPolytopeM[M, V, A] = HPolytopeM(zeros[M](0, d), zeros[V](0), zeros[M](0, d), zeros[V](0))

  def intersection[M, V, @sp(Double) A](hPolytopes: HPolytopeM[M, V, A]*)(implicit alg: AlgMVF[M, V, A]): HPolytopeM[M, V, A] =
    (hPolytopes.head /: hPolytopes.tail) {
      case (prev, current) =>
        HPolytopeM(
          vertcat(prev.mA, current.mA),
          cat(prev.vb, current.vb),
          vertcat(prev.mAeq, current.mAeq),
          cat(prev.vbeq, current.vbeq)
        )
    }*/

  def fromLinearConstraints[M, V, @sp(Double, Long) A: Ring](dim: Int, constraints: Seq[LinearConstraint[V, A]])(implicit alg: AlgMVF[M, V, A]): HPolytopeM[M, V, A] = {
    val ineqs = constraints
      .filter(_.isInstanceOf[LinearInequality[V, A]])
      .map(_.asInstanceOf[LinearInequality[V, A]])
    val eqs = constraints
      .filter(_.isInstanceOf[LinearEquality[V, A]])
      .map(_.asInstanceOf[LinearEquality[V, A]])
    val mAeq = MatBuilder[M, A].fromRows(dim, eqs.map(_.lhs): _*)
    val vbeq = VecBuilder[V, A].build(eqs.map(_.rhs): _*)
    val ineqRows = ineqs.map {
      case li: LinearInequalityLE[V, A] => li.lhs
      case li: LinearInequalityGE[V, A] => -li.lhs
    }
    val mA = MatBuilder[M, A].fromRows(dim, ineqRows: _*)
    val vb = VecBuilder[V, A].build(ineqs.map {
      case li: LinearInequalityLE[V, A] => li.rhs
      case li: LinearInequalityGE[V, A] => -li.rhs
    }: _*)
    HPolytopeM(mA, vb, mAeq, vbeq)
  }
}

final class HPolytopeMImpl[M, V, @sp(Double, Long) A](val mA: M, val vb: V, val mAeq: M, val vbeq: V)(implicit val alg: AlgMVF[M, V, A]) extends HPolytopeM[M, V, A]
