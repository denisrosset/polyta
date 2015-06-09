package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import spire.math.Rational
import spire.syntax.cfor._
import spire.syntax.vectorSpace._
import spire.util._

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

import net.alasc.algebra._
import net.alasc.math.{Perm, Grp}

import solvers._

/** Polyhedron, i.e. possibly unbounded intersection of half-spaces, a set described
  * by inequality and equality constraints.
  */
trait HPolyhedron[V, @sp(Double, Long) A] extends LinearConvexSet[V, A] { lhs =>
  override def toString = constraints.mkString("\n")
  def constraints: Seq[LinearConstraint[V, A]] = facets ++ equalities
  def facets: Seq[LinearInequality[V, A]]
  def equalities: Seq[LinearEquality[V, A]]
  def nX: Int
  //  def contains(v: V)(implicit AO: ApproxOrder[A]): Boolean
  // TODO: type class syntax
//  def symmetric(implicit S: SymmetryFinder[HPolyhedron[V, A], SymHPolyhedron[V, A]]): SymHPolyhedron[V, A] = S.symmetric(lhs)
  def toV[VP](implicit C: HConverter[HPolyhedron[V, A], VP]): VP = C.toV(lhs)
}

final class HPolyhedronImpl[V, @sp(Double, Long) A](val facets: Seq[LinearInequality[V, A]], val equalities: Seq[LinearEquality[V, A]])(implicit alg: AlgVF[V, A]) extends HPolyhedron[V, A] {
  require(facets.nonEmpty)
  override def toString = constraints.mkString("\n")
  def nX = facets.head.lhs.length
}


object HPolyhedron {
  def apply[V, A](facets: Seq[LinearInequality[V, A]], equalities: Seq[LinearEquality[V, A]])(implicit alg: AlgVF[V, A]): HPolyhedron[V, A] = new HPolyhedronImpl(facets, equalities)
}
