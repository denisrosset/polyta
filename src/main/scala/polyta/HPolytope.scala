package com.faacets
package polyta

import spire.algebra._

import net.alasc.finite.Grp

import scalin.immutable.Vec

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
    import A.{IVec, IMat}
    val ineqSatisfied: Seq[(Vec[A], A)] = onFacets.map { facet =>
      val ineq = facet.inequality
      (ineq.lhs, ineq.rhs)
    }
    val eqSatisfied: Iterable[(Vec[A], A)] = equalities.map( eq => (eq.lhs, eq.rhs) )
    val satisfied = (ineqSatisfied ++ eqSatisfied).toSeq
    val newA = IMat.tabulate(satisfied.size, dim) { (r, c) => satisfied(r)._1(c) }
    val newb = IVec.tabulate(satisfied.size)( i => satisfied(i)._2 )
    newA.luDecomposition.solve(newb).to[IVec]
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
