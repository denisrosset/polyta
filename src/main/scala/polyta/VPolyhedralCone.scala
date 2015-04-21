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

/** Polyhedral cone as a set of points that are convex combinations of rays.
  */
trait VPolyhedralCone[V, @sp(Double) A] extends VPolyhedron[V, A] {
  override def toString =
    "\nVertices:\n" + vertices.mkString("\n") + "Rays:\n" + rays.mkString("\n") + "\n"
  def vertices: IndexedSeq[V] = IndexedSeq.empty[V]
  def rays: IndexedSeq[V]

  def nX: Int
}
