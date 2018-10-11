package com.faacets
package polyta

import spire.algebra.CRing
import scalin.immutable.Mat
import scalin.immutable.dense._

/** V-representation polytope, such that any point can be written as:
  *
  * x^T = y^T mV + z^T mR, where y, z >= 0 and sum_i y_i = 1.
  */
case class VPolytope[A](
                         /** Matrix whose rows are vertices of the polytope. */
                         mV: Mat[A],
                         /** Matrix whose rows are rays of the polytope. */
                         mR: Mat[A]
                       ) { lhs =>

  require(mV.nCols == mR.nCols)

  def dim = mV.nCols

  def union(rhs: VPolytope[A]): VPolytope[A] = VPolytope(mV.horzcat(rhs.mV), mR.horzcat(rhs.mR))

  /*
  def facetOnVertices(onVertices: Set[Int], satisfying: Int)(implicit A: Field[A], PA: Pivot[A]): (Vec[A], A) = {
    val seq = onVertices.toSeq
    val zeroH = mV(seq.head, ::)
    val nonZeroVertex = mV(satisfying, ::) - zeroH
    val ortho = Mat.tabulate(onVertices.size, dim) {
      (r, c) => if (r < onVertices.size - 1) mV(seq(r), c) - zeroH(c) else nonZeroVertex(c)
    }.orthogonalized
    val lhs = ortho(ortho.nRows - 1, ::)
    val rhs = lhs.dot(zeroH)
    if (lhs.dot(nonZeroVertex) < rhs)
      LinearInequality(lhs, ComparisonOp.LE,  rhs)
    else
      LinearInequality(-lhs, ComparisonOp.LE, -rhs)
  }

  def affineSpan(implicit A: LinAlg[A]): HPolytope.Aux[A, Symmetry.Without.type] = {
    require(mR.nRows == 0)
    import A.{IMat, IVec, fieldA}
    val basis = tabulate(mV.nRows - 1, dim) {
      (r, c) => mV(r + 1, c) - mV(0, c)
    }.orthogonalized
    val full = (basis vertcat eye(dim)).orthogonalized
    val mAeq = full(basis.nRows until full.nRows, ::)
    val vbeq = mAeq * mV(0, ::)
    HPolytope.fromEqualities(mAeq, vbeq)
  }
*/
}

object VPolytope {

  def empty[A](dim: Int)(implicit A: CRing[A]): VPolytope[A] = {
    val mV = Mat.zeros[A](0, dim)
    val mR = Mat.zeros[A](0, dim)
    VPolytope[A](mV, mR)
  }

  def apply[A](mV: Mat[A])(implicit A: CRing[A]): VPolytope[A] = {
    val d = mV.nCols
    val mR = Mat.zeros[A](0, d)
    VPolytope[A](mV, mR)
  }

  def fromVertices[A](allVertexPoints: Mat[A])(implicit A: CRing[A]): VPolytope[A] =
    VPolytope(allVertexPoints)

  def fromRays[A](allRayPoints: Mat[A])(implicit A: CRing[A]): VPolytope[A] = {
    val d = allRayPoints.nCols
    val mV = Mat.zeros[A](0, d)
    VPolytope[A](mV, allRayPoints)
  }

}
