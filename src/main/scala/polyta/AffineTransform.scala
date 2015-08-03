package com.faacets
package polyta

import scala.{specialized => sp}

import spire.syntax.cfor._

import qalg.algebra._
import qalg.algos._
import qalg.syntax.indup.all._
import qalg.syntax.algos.all._
import qalg.syntax.all._

import net.alasc.algebra._

trait AffineTransform[M, V, @sp(Double, Long) A] extends WithVariables {
  implicit val pack: PackRing.ForMV[M, V, A]
  def mA: M
  def vb: V
  def nX = mA.nRows
  require(nX == mA.nCols)
  require(nX == vb.length)

  override def toString: String = (0 until nX).map( r => mA(r, ::).toString + ", " + vb(r) ).mkString("\n")
}

object AffineTransform {
  final class Impl[M, V, @sp(Double, Long) A](val mA: M, val vb: V)(implicit val pack: PackRing.ForMV[M, V, A]) extends AffineTransform[M, V, A]

  def apply[M, V, @sp(Double, Long) A](mA: M, vb: V)(implicit pack: PackRing.ForMV[M, V, A]) = new Impl(mA, vb)

  // TODO: check it is the right direction
  def fromPermutation[M, V, @sp(Double, Long) A, G](dim: Int, g: G)(implicit pack: PackRing.ForMV[M, V, A], G: PermutationAction[G]): AffineTransform[M, V, A] = {
    val b = MatBuild[M, A].builder(dim, dim)
    cforRange(0 until dim) { c =>
      val r = G.actr(c, g)
      b.add(r, c, pack.A.one)
    }
    val mA = b.result()
    val vb = zeros[V](dim)
    apply(mA, vb)
  }
}
