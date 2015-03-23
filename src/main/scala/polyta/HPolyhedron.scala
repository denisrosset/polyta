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
  override def toString = s"HPolyhedron($mA, $vb, $mAeq, $vbeq)"
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
  def apply[M, V, @sp(Double) A](mA0: M, vb0: V, mAeq0: M, vbeq0: V)(implicit MV0: MatVecInField[M, V, A]): HPolyhedron[M, V, A] =
    new HPolyhedron[M, V, A] {
      def MV = MV0
      def mA = mA0
      def vb = vb0
      def mAeq = mAeq0
      def vbeq = vbeq0
    }
}
