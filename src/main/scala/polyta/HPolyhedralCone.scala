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

/** Polyhedral cone as a set of points statisfying a finite number of homogenous
  * equality and inequality constraints.
  */
trait HPolyhedralCone[V, @sp(Double, Long) A] extends HPolyhedron[V, A] {
  override def constraints: IndexedSeq[LinearConstraintHom[V, A]] = inequalities ++ equalities
  def equalities: IndexedSeq[LinearEqualityHom[V, A]]
  def inequalities: IndexedSeq[LinearInequalityHom[V, A]]
  def nX: Int
}
