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

/** Polyhedron, i.e. possibly unbounded intersection of half-spaces, given by the
  * equations:
  * 
  * - mA * x <= vb
  * - mAeq * x = vbeq
  */
trait HPolyhedron[M, V, @sp(Double) A] extends LinearConvexSet[M, V, A] {
  override def toString =
    ((0 until nIneqs).map( r => mA(r,::).toString + " <= " + vb(r).toString ) ++
      (0 until nEqs).map( r => mAeq(r,::).toString + " == " + vbeq(r).toString )).mkString("\n")
  def mA: M
  def vb: V
  def mAeq: M
  def vbeq: V
  require(nX == mAeq.nCols)
  def nX: Int = mA.nCols
  def nIneqs: Int = mA.nRows
  def nEqs: Int = mAeq.nRows
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
  }
}

object HPolyhedron {
  @inline protected def build[M, V, A](mA0: M, vb0: V, mAeq0: M, vbeq0: V)(implicit M0: MatVecInField[M, V, A]): HPolyhedron[M, V, A] =
    new HPolyhedron[M, V, A] {
      def M = M0
      def mA = mA0
      def vb = vb0
      def mAeq = mAeq0
      def vbeq = vbeq0
    }

  def fromEqualities[M, V, @sp(Double) A](mAeq: M, vbeq: V)(implicit M: MatVecInField[M, V, A]): HPolyhedron[M, V, A] = {
    val nX = mAeq.nCols
    import M.{V, scalar}
    apply(mA = M.zeros(0, nX), vb = V.zeros(0), mAeq = mAeq, vbeq = vbeq)
  }

  def fromInequalities[M, V, @sp(Double) A](mA: M, vb: V)(implicit M: MatVecInField[M, V, A]): HPolyhedron[M, V, A] = {
    val nX = mA.nCols
    import M.{V, scalar}
    apply(mA = mA, vb = vb, mAeq = M.zeros(0, nX), vbeq = V.zeros(0))
  }

  def apply[M, V, @sp(Double) A](mA: M, vb: V, mAeq: M, vbeq: V)(implicit M: MatVecInField[M, V, A]): HPolyhedron[M, V, A] = build(mA, vb, mAeq, vbeq)

  def empty[M, V, @sp(Double) A](d: Int)(implicit M: MatVecInField[M, V, A]): HPolyhedron[M, V, A] = {
    import M.{V, scalar}
    apply(M.zeros(0, d), V.zeros(0), M.zeros(0, d), V.zeros(0))
  }

  def intersection[M, V, @sp(Double) A](d: Int, hpolys: HPolyhedron[M, V, A]*)(implicit M: MatVecInField[M, V, A]): HPolyhedron[M, V, A] = {
    import M.V
    if (hpolys.isEmpty) HPolyhedron.empty(d) else 
      (hpolys.head /: hpolys.tail) {
        case (prev, current) =>
          HPolyhedron(
            vertcat(prev.mA, current.mA),
            cat(prev.vb, current.vb),
            vertcat(prev.mAeq, current.mAeq),
            cat(prev.vbeq, current.vbeq)
          )
      }
  }
}
