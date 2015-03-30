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
  def vertices: M
  def rays: M
  def nX: Int = rays.nCols
  require(nX == vertices.nCols)
  def nRays: Int = rays.nRows
  def nVertices: Int = vertices.nRows
}

object VPolyhedron {
  def fromRays[M, V, @sp(Double) A](rays: M)(implicit MV0: MatVecInField[M, V, A]): VPolyhedron[M, V, A] = {
    import MV0.scalar
    val vertices = MV0.zeros(0, rays.nCols)
    apply(vertices, rays)
  }

  def fromVertices[M, V, @sp(Double) A](vertices: M)(implicit MV0: MatVecInField[M, V, A]): VPolyhedron[M, V, A] = {
    import MV0.scalar
    val rays = MV0.zeros(0, vertices.nCols)
    apply(vertices, rays)
  }

  def apply[M, V, @sp(Double) A](vertices0: M, rays0: M)(implicit MV0: MatVecInField[M, V, A]): VPolyhedron[M, V, A] =
    new VPolyhedron[M, V, A] {
      def MV = MV0
      def vertices = vertices0
      def rays = rays0
    }
}
