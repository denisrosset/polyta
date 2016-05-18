package com.faacets
package polyta

import scalin.immutable.{Mat, Vec}
import scalin.syntax.all._

import spire.syntax.order._
import spire.syntax.field._

/** V-representation polytope, such that any point can be written as:
  *
  * x^T = y^T mV + z^T mR, where y, z >= 0 and sum_i y_i = 1.
  */
trait VPolytope[A] { lhs =>

  /** Matrix whose rows are vertices of the polytope. */
  def mV: Mat[A]

  /** Matrix whose rows are rays of the polytope. */
  def mR: Mat[A]

  require(mV.nCols == mR.nCols)

  type S <: Symmetry

  def symmetry: S

  def dim = mV.nCols

  def union(rhs: VPolytope.Aux[A, S])(implicit A: LinAlg[A], U: VPolytope.Union[S]): VPolytope.Aux[A, S] =
    U[A](lhs, rhs)

  def facetOnVertices(onVertices: Set[Int], satisfying: Int)(implicit A: LinAlg[A]): LinearInequality[A] = {
    import A.{fieldA, orderA}
    import A.{IMat, IVec}
    val seq = onVertices.toSeq
    val zeroH = mV(seq.head, ::)
    val nonZeroVertex = mV(satisfying, ::) - zeroH
    val ortho = tabulate(onVertices.size, dim) {
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

}

object VPolytope {

  implicit def convertSymmetry[A, S <: Symmetry, S1 <: Symmetry]
    (lhs: VPolytope.Aux[A, S])
    (implicit A: LinAlg[A], C: VPolytope.SymmetryConversion[A, S, S1]): VPolytope.Aux[A, S1] =
      C(lhs)

  type Aux[A, S0 <: Symmetry] = VPolytope[A] { type S = S0 }

  trait Union[S <: Symmetry] {

    def apply[A](lhs: VPolytope.Aux[A, S], rhs: VPolytope.Aux[A, S])(implicit A: LinAlg[A]): VPolytope.Aux[A, S]

  }

  object Union {

    implicit object withoutSymmetry extends Union[Symmetry.Without.type] {

      def apply[A](lhs: VPolytope.Aux[A, Symmetry.Without.type], rhs: VPolytope.Aux[A, Symmetry.Without.type])
                  (implicit A: LinAlg[A]): VPolytope.Aux[A, Symmetry.Without.type] = {
        import A.{IMat, IVec}
        val mV1 = lhs.mV vertcat rhs.mV
        val mR1 = lhs.mR vertcat rhs.mR
        VPolytope(mV1, mR1)

      }
    }

  }

  trait SymmetryConversion[A, S <: Symmetry, S1 <: Symmetry] {

    def apply(lhs: VPolytope.Aux[A, S])(implicit A: LinAlg[A]): VPolytope.Aux[A, S1]

  }

  object SymmetryConversion {

    implicit def anyToWithout[A, S <: Symmetry]: SymmetryConversion[A, S, Symmetry.Without.type] =
      new SymmetryConversion[A, S, Symmetry.Without.type] {

        def apply(lhs: VPolytope.Aux[A, S])(implicit A: LinAlg[A]): VPolytope.Aux[A, Symmetry.Without.type] =
          new VPolytope[A] {
            type S = Symmetry.Without.type
            def symmetry = Symmetry.Without
            val mV = lhs.mV
            val mR = lhs.mR
          }
      }

  }

  def apply[A, S0 <: Symmetry](mV0: Mat[A], mR0: Mat[A], symmetry0: S0)
                              (implicit A: LinAlg[A]): VPolytope.Aux[A, S0] =
    new VPolytope[A] {
      type S = S0
      def symmetry = symmetry0
      def mV = mV0
      def mR = mR0
    }

  def apply[A](mV: Mat[A], mR: Mat[A])(implicit A: LinAlg[A]): VPolytope.Aux[A, Symmetry.Without.type] =
    apply(mV, mR, Symmetry.Without)

  def fromRays[A](allRayPoints: Mat[A])(implicit A: LinAlg[A]): VPolytope.Aux[A, Symmetry.Without.type] = {
    import A.IMat
    apply(zeros[A](0, allRayPoints.nCols), allRayPoints)
  }

  def fromVertices[A](allVertexPoints: Mat[A])(implicit A: LinAlg[A]): VPolytope.Aux[A, Symmetry.Without.type] = {
    import A.IMat
    apply(allVertexPoints, zeros[A](0, allVertexPoints.nCols))
  }

  def empty[A](dim: Int)(implicit A: LinAlg[A]): VPolytope.Aux[A, Symmetry.Without.type] = {
    import A.IMat
    apply(zeros[A](0, dim), zeros[A](0, dim))
  }

}