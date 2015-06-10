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
  polyhedron: VPolyhedron[V, Rational],
  rayCols: Set[Int],
  symmetryInfo: Option[SymmetryInfo] = None)

object ExtData {
  def fromPolyhedron[V](polyhedron: VPolyhedron[V, Rational]): ExtData[V] = {
    val rayCols = (polyhedron.vertices.size until (polyhedron.vertices.size + polyhedron.rays.size)).toSet
    ExtData(polyhedron, rayCols)
  }
  def fromSymPolytope[G, V](polytope: SymVPolytope[G, V, Rational]): ExtData[V] = {
    val gens = polytope.symmetryGroup.generators.map(g => Perm.fromImages(polytope.vertexRepresentation.images(g))).toSeq
    val order = polytope.symmetryGroup.order
    val symInfo = SymmetryInfo(false, Some(order), gens, Seq.empty[Int])
    ExtData(polytope, Set.empty[Int], Some(symInfo))
  }
  implicit def FormatRead[V](implicit alg: AlgVF[V, Rational]): FormatRead[ExtData[V]] = new ExtDataRead[V]
  implicit def FormatWrite[V](implicit alg: AlgVF[V, Rational]): FormatWrite[ExtData[V]] = new ExtDataWrite[V]
}
