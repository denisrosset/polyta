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

/** Polyhedral cone defined as the conic hull of finitely many
  * generating points (i.e. columns in the generating matrix `mR`).
  */
trait VPolyhedralConeM[M, V, @sp(Double, Long) A] extends VPolyhedralCone[V, A] { self =>
  implicit def M: MatVecInField[M, V, A]
  implicit def V: VecInField[V, A] = M.V
  implicit override def A: Field[A] = M.scalar

  /** Matrix of generating points, represented as column vectors. */
  def mR: M

  def rays: IndexedSeq[V] = new IndexedSeq[V] {
    def length: Int = self.mR.nCols
    def apply(c: Int): V = self.mR(::, c)
  }

  def nX: Int = mR.nRows
}

/*
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
        apply(vertcat(prev.mV, current.mV), vertcat(prev.mR, current.mR))
    }

  def fromRays[M, V, @sp(Double) A](mR: M)(implicit M: MatVecInField[M, V, A]): VPolyhedronM[M, V, A] = {
    import M.scalar
    val mV = M.zeros(0, mR.nCols)
    apply(mV, mR)
  }

  def fromVertices[M, V, @sp(Double) A](mV: M)(implicit M: MatVecInField[M, V, A]): VPolyhedronM[M, V, A] = {
    import M.scalar
    val mR = M.zeros(0, mV.nCols)
    apply(mV, mR)
  }

  def apply[M, V, @sp(Double) A](mV: M, mR: M)(implicit M: MatVecInField[M, V, A]): VPolyhedronM[M, V, A] = build(mV, mR)

  def empty[M, V, @sp(Double) A](d: Int)(implicit M: MatVecInField[M, V, A]): VPolyhedronM[M, V, A] = {
    import M.{V, scalar}
    apply(M.zeros(0, d), M.zeros(0, d))
  }
}
 */
