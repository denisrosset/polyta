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

trait FacetBase[V, @sp(Double, Long) A] extends Any {
  def inequality: LinearInequality[V, A]
  def representatives: Iterable[FacetBase[V, A]]
}

final class SingleFacet[V, @sp(Double, Long) A](val inequality: LinearInequality[V, A]) extends FacetBase[V, A] {
  def representatives = Iterable(this)
}

/** Polytope, i.e. possibly intersection of half-spaces, a set described
  * by inequality and equality constraints.
  */
trait HPolytope[V, @sp(Double, Long) A] extends Polytope[V, A] {
  implicit val pack: PackField.ForV[V, A]

  override def toString = (facets.map(_.toString) ++ equalities.map(_.toString)).mkString("\n")

  type Facet <: FacetBase[V, A]
  type Equality = LinearEquality[V, A]

  def facets: Seq[Facet]
  def equalities: Seq[Equality]

  /** Action of the symmetry group on facets. */
  implicit def action: Action[Facet, G]
  /** Subgroup of `symGroup` leaving the facet `f` invariant. */
  def symSubgroup(f: Facet): Grp[G]

  def vertexOn(facetIndices: Set[Int]): V = {
    type M = pack.M
    val M = pack.M
    val ineqSatisfied: Seq[(V, A)] = facetIndices.toSeq.map {
      i =>
      val ineq = facets(i).inequality
      (ineq.lhs, ineq.rhs)
    }
    val eqSatisfied: Seq[(V, A)] = equalities.map( eq => (eq.lhs, eq.rhs) )
    val satisfied = ineqSatisfied ++ eqSatisfied
    val newA: M = M.fromRows(nX, M.defaultOptions)(satisfied.map(_._1): _*)
    val newb: V = VecBuild[V, A].build(satisfied.map(_._2): _*)
    pack.MLU.lu(newA).solveV(newb)
  }

  def flatten: HPolytope[V, A] = {
    val inequalities: Seq[LinearInequality[V, A]] = facets.flatMap { facet =>
      val subgrp = symSubgroup(facet)
      val cosets = subgrp \ symGroup
      cosets.iterator.map { coset => (facet <|+| coset.g).inequality }
    }
    HPolytope(nX, inequalities, equalities)
  }
}

final class HPolytopeNoSym[M, V, @sp(Double, Long) A](val mA: M, val vb: V, val mAeq: M, val vbeq: V)(implicit val pack: PackField.ForMV[M, V, A]) extends HPolytope[V, A] {
  require(mA.nCols == mAeq.nCols)
  def nX = mA.nCols
  def facets: Seq[Facet] = new IndexedSeq[Facet] {
    def length = mA.nRows
    def apply(i: Int): Facet = new Facet(LinearInequality(mA(i, ::), LE, vb(i)))
  }
  def equalities: Seq[Equality] = new IndexedSeq[Equality] {
    def length = mAeq.nRows
    def apply(i: Int): Equality = LinearEquality(mAeq(i, ::), vbeq(i))
  }
  object action extends Action[Facet, Unit] {
    def actl(g: Unit, f: Facet): Facet = f
    def actr(f: Facet, g: Unit): Facet = f
  }
  type Facet = SingleFacet[V, A]
  type G = Unit
  val symGroup: Grp[Unit] = Grp(())
  def symSubgroup(f: Facet): Grp[G] = Grp(())
  override def flatten: HPolytope[V, A] = this
}


/*
final class HPolyhedronImpl[V, @sp(Double, Long) A](val facets: Seq[LinearInequality[V, A]], val equalities: Seq[LinearEquality[V, A]])(implicit alg: AlgVF[V, A]) extends HPolyhedron[V, A] {
  require(facets.nonEmpty)
  override def toString = constraints.mkString("\n")
  def nX = facets.head.lhs.length
}
 */

object HPolytope {
  def apply[M, V, A](mA: M, vb: V, mAeq: M, vbeq: V)(implicit pack: PackField.ForMV[M, V, A]): HPolytopeNoSym[M, V, A] = new HPolytopeNoSym(mA, vb, mAeq, vbeq)
  def apply[V, A](nX: Int, facets: Seq[LinearInequality[V, A]], equalities: Seq[LinearEquality[V, A]])(implicit pack: PackField.ForV[V, A]): HPolytope[V, A] = {
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
    new HPolytopeNoSym(mA, vb, mAeq, vbeq)(pack)
  }
}
