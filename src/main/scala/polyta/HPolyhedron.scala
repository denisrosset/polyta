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
  def constraints: Seq[LinearConstraint[V, A]] = inequalities ++ equalities
  def equalities: Seq[LinearEquality[V, A]]
  def inequalities: Seq[LinearInequality[V, A]]
  def nX: Int
  //  def contains(v: V)(implicit AO: ApproxOrder[A]): Boolean
  // TODO: type class syntax
  def symmetric(implicit S: SymmetryFinder[HPolyhedron[V, A], SymHPolyhedron[V, A]]): SymHPolyhedron[V, A] = S.symmetric(lhs)
  def toV[VP](implicit C: HConverter[HPolyhedron[V, A], VP]): VP = C.toV(lhs)
}

object HPolyhedron {
  @inline protected def build[V, A](inequalities0: Seq[LinearInequality[V, A]], equalities0: Seq[LinearEquality[V, A]])(implicit alg0: AlgVF[V, A]): HPolyhedron[V, A] =
    new HPolyhedron[V, A] {
      def alg = alg0
      def inequalities = inequalities0
      def equalities = equalities0
      def nX = inequalities.head.lhs.length
    }
  def apply[V, A](inequalities: Seq[LinearInequality[V, A]], equalities: Seq[LinearEquality[V, A]])(implicit alg: AlgVF[V, A]): HPolyhedron[V, A] = build(inequalities, equalities)
}
