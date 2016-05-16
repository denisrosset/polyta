package com.faacets.polyta

import scalin.immutable.{Mat, Vec}

case class AffineTransform[A](mA: Mat[A], vb: Vec[A]) {

  def dim = mA.nRows

  require(dim == mA.nCols)
  require(dim == vb.length)

// TODO  override def toString: String = (0 until nX).map( r => mA(r, ::).toString + ", " + vb(r) ).mkString("\n")

}

object AffineTransform {


/*  // TODO: check it is the right direction
  def fromPermutation[A, G](dim: Int, g: G)(implicit G: PermutationAction[G]): AffineTransform[A] = {
    val b = matRing MatBuild[M, A].builder(dim, dim)
    cforRange(0 until dim) { c =>
      val r = G.actr(c, g)
      b.add(r, c, pack.A.one)
    }
    val mA = b.result()
    val vb = zeros[V](dim)
    apply(mA, vb)
  }*/

}
