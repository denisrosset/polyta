package com.faacets
package polyta

import spire.algebra.{CRing, Field}
import scalin.immutable.{Mat, Vec}
import scalin.Pivot
import scalin.immutable.dense._

/** Polytope in the H representation. The facets are described using mA and vb, such that
  * mA * x <= vb.
  * Equalities are described using mAeq, vbeq, such that mAeq * x = vbeq.
  */
case class HPolytope[A](
                         /** Matrix describing the left-hand side of the inequalities system. */
                         mA: Mat[A],
                         /** Vector describing the right-hand side of the inequalities system. */
                         vb: Vec[A],
                         /** Matrix describing the left-hand side of the equalities system. */
                         mAeq: Mat[A],
                         /** Vector describing the right-hand side of the equalities system. */
                         vbeq: Vec[A]
                       ) { lhs =>

  require(mA.nCols == mAeq.nCols)
  require(vb.length == mA.nRows)
  require(vbeq.length == mAeq.nRows)

  def dim: Int = mA.nCols

  def intersect(rhs: HPolytope[A]): HPolytope[A] =
    HPolytope(mA.vertcat(rhs.mA), vb.cat(rhs.vb), mAeq.vertcat(rhs.mAeq), vbeq.cat(rhs.vbeq))

  def vertexOn(onFacets: Set[Int])(implicit A: Field[A], PA: Pivot[A]): Vec[A] = {
    val indSeq = onFacets.toSeq
    val ineqA = Mat.tabulate[A](indSeq.size, dim) { (r, c) => mA(indSeq(r), c) }
    val ineqB = Vec.tabulate[A](indSeq.size) { r => vb(indSeq(r)) }
    val newA = ineqA.vertcat(mAeq)
    val newb = ineqB.cat(vbeq)
    solveLinear(newA, newb)
  }

}

object HPolytope {

  def full[A](d: Int)(implicit A: CRing[A]): HPolytope[A] = {
    val mA = Mat.zeros[A](0, d)
    val vb = Vec.zeros[A](0)
    val mAeq = Mat.zeros[A](0, d)
    val vbeq = Vec.zeros[A](0)
    HPolytope(mA, vb, mAeq, vbeq)
  }

  def apply[A](mA: Mat[A], vb: Vec[A])(implicit A: CRing[A]): HPolytope[A] = {
    val d = mA.nCols
    val mAeq = Mat.zeros[A](0, d)
    val vbeq = Vec.zeros[A](0)
    HPolytope(mA, vb, mAeq, vbeq)
  }

  def fromEqualities[A](mAeq: Mat[A], vbeq: Vec[A])(implicit A: CRing[A]): HPolytope[A] = {
    val d = mAeq.nCols
    val mA = Mat.zeros[A](0, d)
    val vb = Vec.zeros[A](0)
    HPolytope(mA, vb, mAeq, vbeq)
  }

  def fromInequalities[A](mA: Mat[A], vb: Vec[A])(implicit A: CRing[A]): HPolytope[A] =
    HPolytope(mA, vb)
}
