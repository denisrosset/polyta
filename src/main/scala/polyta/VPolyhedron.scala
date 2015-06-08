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

/** Polyhedron described by extremal rays and vertices, which are stored as
  * row vectors.
  */
trait VPolyhedron[V, @sp(Double) A] extends LinearConvexSet[V, A] { lhs =>
  override def toString =
    "\nVertices:\n" + vertices.mkString("\n") + "Rays:\n" + rays.mkString("\n") + "\n"
  def vertices: Seq[V]
  def rays: Seq[V]

  def nX: Int
  // TODO: type class syntax
  def symmetric(implicit S: SymmetryFinder[VPolyhedron[V, A], SymVPolyhedron[V, A]]): SymVPolyhedron[V, A] = S.symmetric(lhs)
  def toH[HP](implicit C: VConverter[VPolyhedron[V, A], HP]): HP = C.toH(lhs)

}

object VPolyhedron {
  @inline protected def build[V, A](vertices0: Seq[V], rays0: Seq[V])(implicit alg0: AlgVF[V, A]): VPolyhedron[V, A] =
    new VPolyhedron[V, A] {
      def alg = alg0
      def vertices = vertices0
      def rays = rays0
      def nX = vertices.head.length
    }
  def apply[V, A](vertices: Seq[V], rays: Seq[V])(implicit alg: AlgVF[V, A]): VPolyhedron[V, A] = build(vertices, rays)
}
