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

/** Vertices and rays are stored as row vectors. */
trait VPolyhedronM[M, V, @sp(Double, Long) A] extends VPolyhedron[V, A] {
  implicit def alg: AlgMVF[M, V, A]

  def vertices: IndexedSeq[V] = new IndexedSeq[V] {
    def length = mV.nCols
    def apply(c: Int): V = mV(::, c)
  }

  def rays: IndexedSeq[V] = new IndexedSeq[V] {
    def length = mR.nCols
    def apply(c: Int): V = mR(::, c)
  }

  def mV: M
  def mR: M

 // def mVt: M = mV.t
//  def mRt: M = mR.t

  def nX: Int = mV.nRows
  require(nX == mR.nRows)
}

/*trait VPolyhedronFromH[M, V, @sp(Double, Long) A] extends VPolyhedron[M, V, A] {
  def hPolyhedron: HPolyhedron[M, V, A]

  def vertexBases: IndexedSeq[Set[Int]]

  def inequalities = 
}*/

object VPolyhedronM {
  @inline protected def build[M, V, @sp(Double) A](mV0: M, mR0: M)(implicit alg0: AlgMVF[M, V, A]): VPolyhedronM[M, V, A] =
    new VPolyhedronM[M, V, A] {
      def alg = alg0
      def mV = mV0
      def mR = mR0
    }

  def union[M, V, @sp(Double) A](vPolys: VPolyhedronM[M, V, A]*)(implicit alg: AlgMVF[M, V, A]): VPolyhedronM[M, V, A] =
    (vPolys.head /: vPolys.tail) {
      case (prev, current) =>
        apply(horzcat(prev.mV, current.mV), horzcat(prev.mR, current.mR))
    }

  def fromRays[M, V, @sp(Double) A](mR: M)(implicit alg: AlgMVF[M, V, A]): VPolyhedronM[M, V, A] = {
    val mV = zeros[M](mR.nRows, 0)
    apply(mV, mR)
  }

  def fromVertices[M, V, @sp(Double) A](mV: M)(implicit alg: AlgMVF[M, V, A]): VPolyhedronM[M, V, A] = {
    val mR = zeros[M](mV.nRows, 0)
    apply(mV, mR)
  }

  def apply[M, V, @sp(Double) A](mV: M, mR: M)(implicit alg: AlgMVF[M, V, A]): VPolyhedronM[M, V, A] = build(mV, mR)

  def empty[M, V, @sp(Double) A](d: Int)(implicit alg: AlgMVF[M, V, A]): VPolyhedronM[M, V, A] = {
    apply(zeros[M](d, 0), zeros[M](d, 0))
  }
}
