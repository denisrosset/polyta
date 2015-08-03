package com.faacets
package polyta
package formats
package sympol

import spire.math.Rational

import net.alasc.math.Perm
import net.alasc.syntax.all._

import qalg.algebra._
import qalg.algos._

case class ExtData[V](
  polyhedron: VPolytope[V, Rational],
  rayCols: Set[Int],
  symmetryInfo: Option[SymmetryInfo] = None)

object ExtData {
  def fromPolytope[V](polytope: VPolytope[V, Rational]): ExtData[V] = {
    val rayCols = (polytope.vertices.size until (polytope.vertices.size + polytope.rays.size)).toSet
    ExtData(polytope, rayCols)
  }
/*  def fromSymPolytope[G, V](polytope: SymVPolytope[G, V, Rational]): ExtData[V] = {
    val gens = polytope.symmetryGroup.generators.map(g => Perm.fromImages(polytope.vertexRepresentation.images(g))).toSeq
    val order = polytope.symmetryGroup.order
    val symInfo = SymmetryInfo(false, Some(order), gens, Seq.empty[Int])
    ExtData(polytope, Set.empty[Int], Some(symInfo))
  }*/
  implicit def FormatRead[V](implicit pack: PackField.ForV[V, Rational]): FormatRead[ExtData[V]] = new ExtDataRead[V]
  implicit def FormatWrite[V](implicit pack: PackField.ForV[V, Rational]): FormatWrite[ExtData[V]] = new ExtDataWrite[V]
}
