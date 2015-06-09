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

/** Polyhedron described by extremal rays and vertices.
  */
trait VPolyhedron[V, @sp(Double) A] extends LinearConvexSet[V, A] { lhs =>

  override def toString =
    "\nVertices:\n" + vertices.mkString("\n") + "Rays:\n" + rays.mkString("\n") + "\n"
  def vertices: Seq[V]
  def rays: Seq[V]

  def nX: Int
  // TODO: type class syntax
//  def symmetric(implicit S: SymmetryFinder[VPolyhedron[V, A], SymVPolyhedron[V, A]]): SymVPolyhedron[V, A] = S.symmetric(lhs)
//  def toH[HP](implicit C: VConverter[VPolyhedron[V, A], HP]): HP = C.toH(lhs)
}

final class VPolyhedronImpl[V, @sp(Double) A](val vertices: Seq[V], val rays: Seq[V])(implicit val alg: AlgVF[V, A]) extends VPolyhedron[V, A] {
  require(vertices.nonEmpty || rays.nonEmpty)

  def nX = if (vertices.nonEmpty) vertices.head.length else rays.head.length
}

object VPolyhedron {
  def apply[V, A](vertices: Seq[V], rays: Seq[V])(implicit alg: AlgVF[V, A]): VPolyhedron[V, A] = new VPolyhedronImpl(vertices, rays)
}
