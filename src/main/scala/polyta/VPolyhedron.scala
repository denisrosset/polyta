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

/** Polyhedron described by extremal rays and vertices, which are stored as
  * row vectors.
  */
trait VPolyhedron[M, V, @sp(Double) A] extends LinearConvexSet[M, V, A] {
  override def toString =
    "\nVertices:\n" + mV.toString + "Rays:\n" + mR.toString + "\n"
  def vertices: IndexedSeq[V]
  def rays: IndexedSeq[V]

  def mV: M
  def mR: M
  def nX: Int
  def nVertices: Int
  def nRays: Int
}

trait VPolyhedronFromMatrices[M, V, @sp(Double, Long) A] extends VPolyhedron[M ,V, A] {
  def vertices: IndexedSeq[V] = new IndexedSeq[V] {
    def length = nVertices
    def apply(r: Int): V = mV(r, ::)
  }

  def rays: IndexedSeq[V] = new IndexedSeq[V] {
    def length = nRays
    def apply(r: Int): V = mR(r, ::)
  }

  def mV: M
  def mR: M

  def nX: Int = mV.nCols
  require(nX == mR.nCols)
  def nVertices: Int = mV.nRows
  def nRays: Int = mR.nRows
}

/*trait VPolyhedronFromH[M, V, @sp(Double, Long) A] extends VPolyhedron[M, V, A] {
  def hPolyhedron: HPolyhedron[M, V, A]

  def vertexBases: IndexedSeq[Set[Int]]

  def inequalities = 
}*/

object VPolyhedron {
  def union[M, V, @sp(Double) A](vPolys: VPolyhedron[M, V, A]*)(implicit M0: MatVecInField[M, V, A]): VPolyhedron[M, V, A] =
    (vPolys.head /: vPolys.tail) {
      case (prev, current) =>
        apply(vertcat(prev.mV, current.mV), vertcat(prev.mR, current.mR))
    }

  def fromRays[M, V, @sp(Double) A](mR: M)(implicit M: MatVecInField[M, V, A]): VPolyhedron[M, V, A] = {
    import M.scalar
    val mV = M.zeros(0, mR.nCols)
    apply(mV, mR)
  }

  def fromVertices[M, V, @sp(Double) A](mV: M)(implicit M: MatVecInField[M, V, A]): VPolyhedron[M, V, A] = {
    import M.scalar
    val mR = M.zeros(0, mV.nCols)
    apply(mV, mR)
  }

  @inline protected def build[M, V, @sp(Double) A](mV0: M, mR0: M)(implicit M0: MatVecInField[M, V, A]): VPolyhedron[M, V, A] =
    new VPolyhedronFromMatrices[M, V, A] {
      def M = M0
      def mV = mV0
      def mR = mR0
    }

  def apply[M, V, @sp(Double) A](mV: M, mR: M)(implicit M: MatVecInField[M, V, A]): VPolyhedron[M, V, A] = build(mV, mR)

  def empty[M, V, @sp(Double) A](d: Int)(implicit M: MatVecInField[M, V, A]): VPolyhedron[M, V, A] = {
    import M.{V, scalar}
    apply(M.zeros(0, d), M.zeros(0, d))
  }
}
