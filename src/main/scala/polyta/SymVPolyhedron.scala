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

trait SymVPolyhedron[V, @sp(Double) A] extends VPolyhedron[V, A] {
  /** Element of the symmetry group of the polyhedron. The first element
    * of the pair acts on vertices, the second on rays. */
  type G = (Perm, Perm)
  def symmetryGroup: Grp[G]
}

object SymVPolyhedron {
  @inline protected def build[V, @sp(Double) A](vertices0: Seq[V], rays0: Seq[V], symmetryGroup0: Grp[(Perm, Perm)])(implicit V0: VecInField[V, A]): SymVPolyhedron[V, A] =
    new SymVPolyhedron[V, A] {
      def V = V0
      def vertices = vertices0
      def rays = rays0
      def symmetryGroup = symmetryGroup0
      def nX = vertices.head.length
    }
  def apply[V, @sp(Double) A](vertices: Seq[V], rays: Seq[V], symmetryGroup: Grp[(Perm, Perm)])(implicit V: VecInField[V, A]): SymVPolyhedron[V, A] =
    build(vertices, rays, symmetryGroup)
  def fromMaps[M, V, @sp(Double) A](vPolyhedron: VPolyhedron[V, A], maps: Iterable[AffineTransform[M, V, A]])(implicit alg: AlgMVF[M, V, A]): SymVPolyhedron[V, A] = {
    ???
  }
}
