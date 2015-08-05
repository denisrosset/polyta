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
  polytope: VPolytope[V, Rational],
  rayCols: Set[Int],
  symmetryInfo: Option[SymmetryInfo] = None)

object ExtData {
  def fromPolytope[V](polytope: VPolytope[V, Rational]): ExtData[V] = {
    val rayCols = (polytope.vertices.size until (polytope.vertices.size + polytope.rays.size)).toSet
    ExtData(polytope, rayCols)
  }

  def fromSymPolytope[V, G](polytope: VPolytopeCombSym[_, V, Rational, G]): ExtData[V] = {
    require(polytope.rays.isEmpty)
    val generators = polytope.symGroup.generators.toSeq
    implicit def action = polytope.vertexIndexAction
    val permutations = generators.map(_.to[Perm])
    val order = polytope.symGroup.order
    val symInfo = SymmetryInfo(false, Some(order), permutations, Seq.empty[Int])
    ExtData(polytope, Set.empty[Int], Some(symInfo))
  }

  implicit def FormatRead[V](implicit pack: PackField.ForV[V, Rational]): FormatRead[ExtData[V]] = new ExtDataRead[V]
  implicit def FormatWrite[V](implicit pack: PackField.ForV[V, Rational]): FormatWrite[ExtData[V]] = new ExtDataWrite[V]
}
