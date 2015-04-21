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

/** Polyhedron, i.e. possibly unbounded intersection of half-spaces, a set described
  * by inequality and equality constraints.
  */
trait HPolyhedron[V, @sp(Double, Long) A] extends LinearConvexSet[V, A] {
  override def toString = constraints.mkString("\n")
  def constraints: IndexedSeq[LinearConstraint[V, A]] = inequalities ++ equalities
  def equalities: IndexedSeq[LinearEquality[V, A]]
  def inequalities: IndexedSeq[LinearInequality[V, A]]
  def nX: Int
//  def contains(v: V)(implicit AO: ApproxOrder[A]): Boolean
}

