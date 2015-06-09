package com.faacets
package polyta

import scala.{specialized => sp}

import spire.algebra._
import spire.math.Rational
import spire.syntax.action._
import spire.syntax.cfor._
import spire.syntax.vectorSpace._
import spire.util._

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

import net.alasc.algebra._
import net.alasc.math.{Perm, Grp}

trait SymVPolytope[G, V, @sp(Double) A] extends VPolytope[V, A] {
  def symmetryGroup: Grp[G]
  def vertexRepresentation: Representation[G]
  def vertexSymmetryGroup(v: Int): Grp[G]
  def vertexFamilies: Seq[Seq[V]]
}

final class SymVPolytopeImpl[G, V, @sp(Double) A](val vertices: Seq[V], val symmetryGroup: Grp[G], val vertexRepresentation: Representation[G])(implicit alg: AlgVF[V, A]) extends SymVPolytope[G, V, A] {
  def nX = vertices.head.length

  def vertexSymmetryGroup(v: Int): Grp[G] = symmetryGroup.stabilizer(v, vertexRepresentation)._1
  def vertexFamilies: Seq[Seq[V]] =
    DomainOrbits.orbits(symmetryGroup, vertexRepresentation)
      .map(_.toSeq.sorted).toSeq.sortBy(_.head).map( seq => seq.map(vertices(_)) )
}

object SymVPolytope {
  def apply[G, V, @sp(Double) A](vertices: Seq[V], symmetryGroup: Grp[G], vertexRepresentation: Representation[G])(implicit alg: AlgVF[V, A]): SymVPolytope[G, V, A] =
    new SymVPolytopeImpl(vertices, symmetryGroup, vertexRepresentation)
}
