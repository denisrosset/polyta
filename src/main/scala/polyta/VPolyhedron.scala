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

/** Polyhedron described by extremal rays and vertices, which are stored as
  * row vectors.
  */
trait VPolyhedron[V, @sp(Double) A] extends LinearConvexSet[V, A] {
  override def toString =
    "\nVertices:\n" + vertices.mkString("\n") + "Rays:\n" + rays.mkString("\n") + "\n"
  def vertices: Seq[V]
  def rays: Seq[V]

  def nX: Int
}

object VPolyhedron {
  @inline protected def build[V, A](vertices0: Seq[V], rays0: Seq[V])(implicit V0: VecInField[V, A]): VPolyhedron[V, A] =
    new VPolyhedron[V, A] {
      def V = V0
      def vertices = vertices0
      def rays = rays0
      def nX = vertices.head.length
    }
  def apply[V, A](vertices: Seq[V], rays: Seq[V])(implicit V: VecInField[V, A]): VPolyhedron[V, A] = build(vertices, rays)
}
