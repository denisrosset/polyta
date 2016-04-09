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

/** Polytope, i.e. possibly intersection of half-spaces, a set described
  * by inequality and equality constraints.
  */
trait HPolytope[A] extends Polytope[A] { lhs =>

  /** Facet type for this polytope type. */
  type Facet <: HPolytope.Facet.ForG[A, G] { type F = Facet }

  /** Type alias for linear equality constraints. */
  type Equality = LinearEquality[A]

  /** Facet representatives under symmetry. */
  def facets: Seq[Facet]

  /** All facet representatives. */
  def allFacets: Iterable[Facet]

  /** Equality constraints. */
  def equalities: Iterable[Equality]

  override def toString = (facets.map(_.toString) ++ equalities.map(_.toString)).mkString("\n")

  /** Action of the symmetry group on facets. */
  implicit def action: Action[Facet, G]

/*  def symmetriesDiscarded: HPolytope[A] = {
    val allInequalities: Seq[LinearInequality[A]] = allFacets.map(_.inequality)
    HPolytope(dim, allInequalities, equalities)
  }*/

  def vertexOn(onFacets: Seq[Facet]): Vec[A] = {
    import A.{IVec, IMat, fieldA, orderA}
    val ineqSatisfied: Seq[(V, A)] = onFacets.map { facet =>
      val ineq = facet.inequality
      (ineq.lhs, ineq.rhs)
    }
    val eqSatisfied: Seq[(Vec[A], A)] = equalities.map( eq => (eq.lhs, eq.rhs) )
    val satisfied = ineqSatisfied ++ eqSatisfied
    val newA: M = M.fromRows(nX, M.defaultOptions)(satisfied.map(_._1): _*)
    val newb: V = VecBuild[V, A].build(satisfied.map(_._2): _*)
    pack.MLU.lu(newA).solveV(newb)
  }

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

}
