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

trait VPolyhedronM[M, V, @sp(Double, Long) A] extends VPolyhedron[V, A] {
  implicit def M: MatVecInField[M, V, A]
  implicit def V: VecInField[V, A] = M.V
  implicit override def A: Field[A] = M.scalar

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
  @inline protected def build[M, V, @sp(Double) A](mV0: M, mR0: M)(implicit M0: MatVecInField[M, V, A]): VPolyhedronM[M, V, A] =
    new VPolyhedronM[M, V, A] {
      def M = M0
      def mV = mV0
      def mR = mR0
    }

  def union[M, V, @sp(Double) A](vPolys: VPolyhedronM[M, V, A]*)(implicit M0: MatVecInField[M, V, A]): VPolyhedronM[M, V, A] =
    (vPolys.head /: vPolys.tail) {
      case (prev, current) =>
        apply(horzcat(prev.mV, current.mV), horzcat(prev.mR, current.mR))
    }

  def fromRays[M, V, @sp(Double) A](mR: M)(implicit M: MatVecInField[M, V, A]): VPolyhedronM[M, V, A] = {
    import M.scalar
    val mV = M.zeros(mR.nRows, 0)
    apply(mV, mR)
  }

  def fromVertices[M, V, @sp(Double) A](mV: M)(implicit M: MatVecInField[M, V, A]): VPolyhedronM[M, V, A] = {
    import M.scalar
    val mR = M.zeros(mV.nRows, 0)
    apply(mV, mR)
  }

  def apply[M, V, @sp(Double) A](mV: M, mR: M)(implicit M: MatVecInField[M, V, A]): VPolyhedronM[M, V, A] = build(mV, mR)

  def empty[M, V, @sp(Double) A](d: Int)(implicit M: MatVecInField[M, V, A]): VPolyhedronM[M, V, A] = {
    import M.{V, scalar}
    apply(M.zeros(d, 0), M.zeros(d, 0))
  }
}
