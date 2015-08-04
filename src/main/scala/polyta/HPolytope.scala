package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import spire.math.Rational
import spire.syntax.cfor._
import spire.syntax.action._
import spire.syntax.vectorSpace._
import spire.util._

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._
import qalg.syntax.indup.all._
import qalg.syntax.algos.all._

import net.alasc.algebra._
import net.alasc.math.{Perm, Grp}
import net.alasc.std.unit._

trait FacetBase[V, @sp(Double, Long) A, G] extends Any {
  override def toString = inequality.toString
  type F <: FacetBase[V, A, G]
  def inequality: LinearInequality[V, A]
  /** Representatives of this facet under symmetry. */
  def representatives: Iterable[F]
  /** Subgroup of the polytope symmetry group leaving this facet invariant. */
  def symSubgroup: Grp[G]
}

/*
final class SingleFacet[V, @sp(Double, Long) A](val inequality: LinearInequality[V, A]) extends FacetBase[V, A] {
  type F = SingleFacet[V, A]
  def representatives = Iterable(this)
}
 */

/** Polytope, i.e. possibly intersection of half-spaces, a set described
  * by inequality and equality constraints.
  */
trait HPolytope[V, @sp(Double, Long) A] extends Polytope[V, A] {
  implicit val pack: PackField.ForV[V, A]
  implicit def orderA: Order[A]
  override def toString = (facets.map(_.toString) ++ equalities.map(_.toString)).mkString("\n")

  /** Facet type for this polytope type. */
  type Facet <: FacetBase[V, A, G] { type F = Facet }
  /** Type alias for linear equality constraints. */
  type Equality = LinearEquality[V, A]

  /** Facet representatives under symmetry. */
  def facets: Seq[Facet]
  /** All facet representatives. */
  def allFacets: Seq[Facet] = facets.flatMap(_.representatives)
  /** Equality constraints. */
  def equalities: Seq[Equality]

  /** Action of the symmetry group on facets. */
  implicit def action: Action[Facet, G]

//  def rayOn(facets: Iterable[Facet], satisfying: Facet)
  def vertexOn(onFacets: Seq[Facet]): V = {
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

  def flatten: HPolytope[V, A] = { // TODO: produce HPolytopeCombSym instead
    val allInequalities: Seq[LinearInequality[V, A]] = allFacets.map(_.inequality)
    HPolytope(nX, allInequalities, equalities)
  }
}

object HPolytope {
  def apply[M, V, A: Order](mA: M, vb: V, mAeq: M, vbeq: V)(implicit pack: PackField.ForMV[M, V, A]): HPolytopeCombSym[M, V, A, Unit] = HPolytopeCombSym(mA, vb, mAeq, vbeq, Grp.trivial[Unit], FaithfulPermutationAction[Unit])
  def apply[V, A: Order](nX: Int, facets: Seq[LinearInequality[V, A]], equalities: Seq[LinearEquality[V, A]])(implicit pack: PackField.ForV[V, A]): HPolytope[V, A] = {
    type M = pack.M
    implicit val M = pack.M
    implicit val V = pack.V
    implicit val A = pack.A
    val mA = M.fromRows(nX)(facets.map {
      facet => if (facet.op == LE) facet.lhs else -facet.lhs
    }: _*)
    val vb = V.build(facets.map {
      facet => if (facet.op == LE) facet.rhs else -facet.rhs
    }: _*)
    val mAeq = M.fromRows(nX, M.defaultOptions)(equalities.map(_.lhs): _*)
    val vbeq = pack.V.build(equalities.map(_.rhs): _*)
    apply(mA, vb, mAeq, vbeq)(Order[A], pack)
  }
}
