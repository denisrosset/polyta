package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra.Field

import qalg.algebra._
import qalg.syntax.all._

import net.alasc.algebra._

trait AffineTransform[M, V, @sp(Double, Long) A] extends WithVariables {
  implicit def M: MatVecInField[M, V, A]
  implicit def V: VecInField[V, A] = M.V
  implicit def A: Field[A] = M.scalar

  def mA: M
  def vb: V
  def nX = mA.nRows
  require(nX == mA.nCols)
  require(nX == vb.length)

  override def toString: String = (0 until nX).map( r => mA(r, ::).toString + ", " + vb(r) ).mkString("\n")
}

object AffineTransform {
  protected def build[M, V, @sp(Double, Long) A, G](mA0: M, vb0: V)(implicit M0: MatVecInField[M, V, A]): AffineTransform[M, V, A] =
    new AffineTransform[M, V, A] {
      def M = M0
      def mA = mA0
      def vb = vb0
    }

  def apply[M, V, @sp(Double, Long) A](mA: M, vb: V)(implicit M: MatVecInField[M, V, A]) = build(mA, vb)

  def fromPermutation[M, V, @sp(Double, Long) A, G](dim: Int, g: G)(implicit M: MatVecInField[M, V, A], G: PermutationAction[G]): AffineTransform[M, V, A] = ???
}
