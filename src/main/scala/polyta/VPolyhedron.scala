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

/** Polyhedron described by extremal rays and vertices, which are stored as
  * row vectors.
  */
trait VPolyhedron[M, V, @sp(Double) A] extends LinearConvexSet[M, V, A] {
  override def toString =
    "Rays:\n" + rays.toString + "\nVertices:\n" + vertices.toString + "\n"
  def vertices: M
  def rays: M
  def nX: Int = rays.nCols
  require(nX == vertices.nCols)
  def nRays: Int = rays.nRows
  def nVertices: Int = vertices.nRows
}

object VPolyhedron {
  def union[M, V, @sp(Double) A](dim: Int, vpolys: VPolyhedron[M, V, A]*)(implicit M0: MatVecInField[M, V, A]): VPolyhedron[M, V, A] =
    if (vpolys.isEmpty) VPolyhedron.empty(dim) else
      (vpolys.head /: vpolys.tail) {
        case (prev, current) =>
          VPolyhedron(vertcat(prev.vertices, current.vertices), vertcat(prev.rays, current.rays))
      }

  def fromRays[M, V, @sp(Double) A](rays: M)(implicit M0: MatVecInField[M, V, A]): VPolyhedron[M, V, A] = {
    import M0.scalar
    val vertices = M0.zeros(0, rays.nCols)
    apply(vertices, rays)
  }

  def fromVertices[M, V, @sp(Double) A](vertices: M)(implicit M0: MatVecInField[M, V, A]): VPolyhedron[M, V, A] = {
    import M0.scalar
    val rays = M0.zeros(0, vertices.nCols)
    apply(vertices, rays)
  }

  @inline protected def build[M, V, @sp(Double) A](vertices0: M, rays0: M)(implicit M0: MatVecInField[M, V, A]): VPolyhedron[M, V, A] =
    new VPolyhedron[M, V, A] {
      def M = M0
      def vertices = vertices0
      def rays = rays0
    }

  def apply[M, V, @sp(Double) A](vertices: M, rays: M)(implicit M: MatVecInField[M, V, A]): VPolyhedron[M, V, A] = build(vertices, rays)

  def empty[M, V, @sp(Double) A](d: Int)(implicit M: MatVecInField[M, V, A]): VPolyhedron[M, V, A] = {
    import M.{V, scalar}
    apply(M.zeros(0, d), M.zeros(0, d))
  }
}
