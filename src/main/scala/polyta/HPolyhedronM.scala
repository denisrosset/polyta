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

trait HPolyhedronM[M, V, @sp(Double, Long) A] extends HPolyhedron[V, A] {
  implicit def alg: AlgMVF[M, V, A]
  implicit override def A: Field[A] = alg.M.A

  def equalities: IndexedSeq[LinearEquality[V, A]] = new IndexedSeq[LinearEquality[V, A]] {
    def length = nEqs
    def apply(r: Int): LinearEquality[V, A] = LinearEquality(mAeq(r, ::), vbeq(r))
  }

  def inequalities: IndexedSeq[LinearInequality[V, A]] = new IndexedSeq[LinearInequality[V, A]] {
    def length = nIneqs
    def apply(r: Int): LinearInequality[V, A] = LinearInequalityLE(mA(r, ::), vb(r))
  }

  def mA: M
  def vb: V
  def mAeq: M
  def vbeq: V

  def nX: Int = mA.nCols
  require(nX == mAeq.nCols)

  def nIneqs: Int = mA.nRows
  def nEqs: Int = mAeq.nRows
/*
  def contains(v: V)(implicit AO: ApproxOrder[A]): Boolean = {
    val ineqTest = mA ::* v - vb
    val eqTest = mAeq ::* v - vbeq
    cforRange(0 until ineqTest.length) { k =>
      if (AO.gt(ineqTest(k), A.zero)) return false
    }
    cforRange(0 until eqTest.length) { k =>
      val c = AO.partialCompare(eqTest(k), A.zero)
      if (c > 0 || c < 0) return false
    }
    true
  }*/
}

object HPolyhedronM {
  @inline protected def build[M, V, A](mA0: M, vb0: V, mAeq0: M, vbeq0: V)(implicit alg0: AlgMVF[M, V, A]): HPolyhedronM[M, V, A] =
    new HPolyhedronM[M, V, A] {
      def alg = alg0
      def mA = mA0
      def vb = vb0
      def mAeq = mAeq0
      def vbeq = vbeq0
    }

  def fromEqualities[M, V, @sp(Double) A](mAeq: M, vbeq: V)(implicit alg: AlgMVF[M, V, A]): HPolyhedronM[M, V, A] =
    apply(mA = zeros[M](0, mAeq.nCols), vb = zeros[V](0), mAeq = mAeq, vbeq = vbeq)

  def fromInequalities[M, V, @sp(Double) A](mA: M, vb: V)(implicit alg: AlgMVF[M, V, A]): HPolyhedronM[M, V, A] =
    apply(mA = mA, vb = vb, mAeq = zeros[M](0, mA.nCols), vbeq = zeros[V](0))

  def apply[M, V, @sp(Double) A](mA: M, vb: V, mAeq: M, vbeq: V)(implicit alg: AlgMVF[M, V, A]): HPolyhedronM[M, V, A] = build(mA, vb, mAeq, vbeq)

  def empty[M, V, @sp(Double) A](d: Int)(implicit alg: AlgMVF[M, V, A]): HPolyhedronM[M, V, A] = HPolyhedronM(zeros[M](0, d), zeros[V](0), zeros[M](0, d), zeros[V](0))

  def intersection[M, V, @sp(Double) A](hPolyhedrons: HPolyhedronM[M, V, A]*)(implicit alg: AlgMVF[M, V, A]): HPolyhedronM[M, V, A] =
    (hPolyhedrons.head /: hPolyhedrons.tail) {
      case (prev, current) =>
        HPolyhedronM(
          vertcat(prev.mA, current.mA),
          cat(prev.vb, current.vb),
          vertcat(prev.mAeq, current.mAeq),
          cat(prev.vbeq, current.vbeq)
        )
    }

  def fromLinearConstraints[M, V, @sp(Double, Long) A: Ring](dim: Int, constraints: Seq[LinearConstraint[V, A]])(implicit alg: AlgMVF[M, V, A]): HPolyhedronM[M, V, A] = {
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
    HPolyhedronM(mA, vb, mAeq, vbeq)
  }
}
