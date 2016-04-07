package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import spire.math.Rational
import spire.syntax.cfor._
import spire.syntax.action._
import spire.syntax.vectorSpace._
import spire.util._

import net.alasc.algebra._
import net.alasc.finite.Grp
import net.alasc.prep.PGrp.default._
import net.alasc.perms.Perm
import net.alasc.std.unit._

import scalin.immutable.{Mat, Vec}

/*
final class SingleFacet[V, @sp(Double, Long) A](val inequality: LinearInequality[V, A]) extends FacetBase[V, A] {
  type F = SingleFacet[V, A]
  def representatives = Iterable(this)
}
 */

/** Polytope, i.e. possibly intersection of half-spaces, a set described
  * by inequality and equality constraints.
  */
trait HPolytope[A] extends Polytope[A] { lhs =>

  type G

  /** Facet type for this polytope type. */
  type Facet <: HPolytope.Facet.ForG[A, G] { type F = Facet }

  /** Type alias for linear equality constraints. */
  type Equality = LinearEquality[A]

  /** Facet representatives under symmetry. */
  def facets: Seq[Facet]

  /** All facet representatives. */
  def allFacets: Seq[Facet] = facets.flatMap(_.representatives)

  /** Equality constraints. */
  def equalities: Seq[Equality]

  override def toString = (facets.map(_.toString) ++ equalities.map(_.toString)).mkString("\n")

  /** Action of the symmetry group on facets. */
  implicit def action: Action[Facet, G]

  def symmetriesDiscarded: HPolytope[A] = {
    val allInequalities: Seq[LinearInequality[A]] = allFacets.map(_.inequality)
    HPolytope(dim, allInequalities, equalities)
  }

//  def rayOn(facets: Iterable[Facet], satisfying: Facet)
/*  def vertexOn(onFacets: Seq[Facet]): V = {
    type M = pack.M
    val M = pack.M
    val ineqSatisfied: Seq[(V, A)] = onFacets.map { facet =>
      val ineq = facet.inequality
      (ineq.lhs, ineq.rhs)
    }
    val eqSatisfied: Seq[(V, A)] = equalities.map( eq => (eq.lhs, eq.rhs) )
    val satisfied = ineqSatisfied ++ eqSatisfied
    val newA: M = M.fromRows(nX, M.defaultOptions)(satisfied.map(_._1): _*)
    val newb: V = VecBuild[V, A].build(satisfied.map(_._2): _*)
    pack.MLU.lu(newA).solveV(newb)
  }

  */
}

object HPolytope {

  trait Facet[A] {
    def inequality: LinearInequality[A]
    override def toString = inequality.toString
    /** Symmetry group element type. */
    type G
    /** Self type. */
    type F <: Facet.ForG[A, G]
    /** Representatives of this facet under symmetry. */
    def representatives: Iterable[F]
    /** Subgroup of the polytope symmetry group leaving this facet invariant. */
    def symSubgroup: Grp[G]
  }

  object Facet {
    type ForG[A, G0] = Facet[A] { type G = G0 }
  }

  def apply[A](mA: Mat[A], vb: Vec[A], mAeq: Mat[A], vbeq: Vec[A])(implicit A: LinAlg[A]): HPolytopeCombSym[A] = HPolytopeCombSym(mA, vb, mAeq, vbeq, Grp.trivial[Perm])

  def apply[A](dim: Int, facets: Seq[LinearInequality[A]], equalities: Seq[LinearEquality[A]])(implicit A: LinAlg[A]): HPolytopeCombSym[A] = {
    import A.{fieldA, IMat, IVec}
    import ComparisonOp._

    val mA = IMat.tabulate(facets.size, dim) { (r, c) =>
      if (facets(r).op == LE) facets(r).lhs(c) else -facets(r).lhs(c)
    }
    val vb = IVec.tabulate(facets.size)( i => if (facets(i).op == LE) facets(i).rhs else -facets(i).rhs )
    val mAeq = IMat.tabulate(equalities.size, dim)( (r, c) => equalities(r).lhs(c) )
    val vbeq = IVec.tabulate(equalities.size)( i => equalities(i).rhs )
    apply(mA, vb, mAeq, vbeq)
  }

}
