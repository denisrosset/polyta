package com.faacets
package polyta

import scalin.immutable.{Mat, Vec}
import scalin.syntax.all._
import spire.syntax.ring._

import com.faacets.polyta.formats.sympol.SymmetryInfo

import net.alasc.finite.Grp
import net.alasc.named.Symmetric
import net.alasc.perms.Perm

/** Polytope in the H representation. The facets are described using `mA` and `vb`, such that
  * `mA * x <= vb`.
  * Equalities are described using `mAeq`, `vbeq`, such that `mAeq * x = vbeq`.
  */
trait HPolytope[A] {
  lhs =>

  /** Matrix describing the left-hand side of the inequalities system. */
  def mA: Mat[A]
  /** Vector describing the right-hand side of the inequalities system. */
  def vb: Vec[A]
  /** Matrix describing the left-hand side of the equalities system. */
  def mAeq: Mat[A]
  /** Vector describing the right-hand side of the equalities system. */
  def vbeq: Vec[A]

  require(mA.nCols == mAeq.nCols)
  require(vb.length == mA.nRows)
  require(vbeq.length == mAeq.nRows)

  type S <: Symmetry

  def symmetry: S

  def dim: Int = mA.nCols

  def vertexOn(onFacets: Set[Int])(implicit A: LinAlg[A]): Vec[A] = {
    import A.{IVec, IMat}
    val indSeq = onFacets.toSeq
    val ineqA = tabulate(indSeq.size, dim) { (r, c) => mA(indSeq(r), c) }
    val ineqB = tabulate(indSeq.size) { r => vb(indSeq(r)) }
    val newA = ineqA vertcat mAeq
    val newb = ineqB cat vbeq
    newA.luDecomposition.solve(newb).toVec[IVec]
  }

  /** Intersects this polytope with another polytope with compatible symmetry information type. */
  def intersect(rhs: HPolytope.Aux[A, S])(implicit A: LinAlg[A], I: HPolytope.Intersect[S]): HPolytope.Aux[A, S] =
    I(lhs, rhs)

}

object HPolytope {

  implicit def convertSymmetry[A, S <: Symmetry, S1 <: Symmetry](lhs: HPolytope.Aux[A, S])(implicit A: LinAlg[A], C: HPolytope.SymmetryConversion[A, S, S1]): HPolytope.Aux[A, S1] =
    C(lhs)


  type Aux[A, S0 <: Symmetry] = HPolytope[A] { type S = S0 }

  trait Intersect[S <: Symmetry] {

    def apply[A](lhs: HPolytope.Aux[A, S], rhs: HPolytope.Aux[A, S])(implicit A: LinAlg[A]): HPolytope.Aux[A, S]

  }

  object Intersect {

    implicit object withoutSymmetry extends Intersect[Symmetry.Without.type] {

      def apply[A](lhs: HPolytope.Aux[A, Symmetry.Without.type], rhs: HPolytope.Aux[A, Symmetry.Without.type])
                   (implicit A: LinAlg[A]): HPolytope.Aux[A, Symmetry.Without.type] = {
        import A.{IMat, IVec}
        val mA1 = lhs.mA vertcat rhs.mA
        val mAeq1 = lhs.mAeq vertcat rhs.mAeq
        val vb1 = lhs.vb cat rhs.vb
        val vbeq1 = lhs.vbeq cat rhs.vbeq
        HPolytope(mA1, vb1, mAeq1, vbeq1)
      }

    }

  }

  trait SymmetryConversion[A, S <: Symmetry, S1 <: Symmetry] {

    def apply(lhs: HPolytope.Aux[A, S])(implicit A: LinAlg[A]): HPolytope.Aux[A, S1]

  }

  object SymmetryConversion {

    implicit def anyToWithout[A, S <: Symmetry]: SymmetryConversion[A, S, Symmetry.Without.type] =
      new SymmetryConversion[A, S, Symmetry.Without.type] {

        def apply(lhs: HPolytope.Aux[A, S])(implicit A: LinAlg[A]): HPolytope.Aux[A, Symmetry.Without.type] = new {
          val mA = lhs.mA
          val vb = lhs.vb
          val mAeq = lhs.mAeq
          val vbeq = lhs.vbeq
        } with HPolytope[A] {
          type S = Symmetry.Without.type
          def symmetry = Symmetry.Without
        }

      }

  }

  def full[A](d: Int)(implicit A: LinAlg[A]): HPolytope.Aux[A, Symmetry.Permutation] = new HPolytope[A] {
    import net.alasc.perms.default._
    def mA = A.IMat.zeros(0, d)
    def vb = A.IVec.zeros(0)
    def mAeq = A.IMat.zeros(0, d)
    def vbeq = A.IVec.zeros(0)
    type S = Symmetry.Permutation
    val symmetry = Symmetry.Permutation(Symmetric(dim))
  }

  def apply[A, S0 <: Symmetry](mA0: Mat[A], vb0: Vec[A], mAeq0: Mat[A], vbeq0: Vec[A], symmetry0: S0)
                              (implicit A: LinAlg[A]): HPolytope.Aux[A, S0] = new {
    val mA = mA0
    val vb = vb0
    val mAeq = mAeq0
    val vbeq = vbeq0

  } with HPolytope[A] {
    type S = S0
    def symmetry = symmetry0
  }

  def apply[A](mA: Mat[A], vb: Vec[A], mAeq: Mat[A], vbeq: Vec[A])
                              (implicit A: LinAlg[A]): HPolytope.Aux[A, Symmetry.Without.type] =
    apply[A, Symmetry.Without.type](mA, vb, mAeq, vbeq, Symmetry.Without)

  def fromEqualities[A](mAeq: Mat[A], beq: Vec[A])(implicit A: LinAlg[A]): HPolytope.Aux[A, Symmetry.Without.type] =
    apply(A.IMat.zeros(0, mAeq.nCols), A.IVec.zeros(0), mAeq, beq)

  def fromInequalities[A](mA: Mat[A], b: Vec[A])(implicit A: LinAlg[A]): HPolytope.Aux[A, Symmetry.Without.type] =
    apply(mA, b, A.IMat.zeros(0, mA.nCols), A.IVec.zeros(0))

  def fromLinearConstraints[A](dim: Int, facets: Seq[LinearInequality[A]], equalities: Seq[LinearEquality[A]])
                              (implicit A: LinAlg[A]): HPolytope.Aux[A, Symmetry.Without.type] = {
    import A.{fieldA, IMat, IVec}
    import ComparisonOp._

    val mA = tabulate(facets.size, dim) { (r, c) =>
      if (facets(r).op == LE) facets(r).lhs(c) else -facets(r).lhs(c)
    }
    val vb = tabulate(facets.size)( i => if (facets(i).op == LE) facets(i).rhs else -facets(i).rhs )
    val mAeq = IMat.tabulate(equalities.size, dim)( (r, c) => equalities(r).lhs(c) )
    val vbeq = IVec.tabulate(equalities.size)( i => equalities(i).rhs )
    apply(mA, vb, mAeq, vbeq)
  }

  def fromLinearConstraints[A](dim: Int, constraints: Seq[LinearConstraint[A]])
                              (implicit A: LinAlg[A]): HPolytope.Aux[A, Symmetry.Without.type] = {
    val ineqs = constraints.collect { case ineq: LinearInequality[A] => ineq }
    val eqs = constraints.collect { case eq: LinearEquality[A] => eq }
    fromLinearConstraints[A](dim, ineqs, eqs)
  }

}
