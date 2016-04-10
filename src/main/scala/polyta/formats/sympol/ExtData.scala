package com.faacets
package polyta
package formats
package sympol

import spire.math.Rational

import net.alasc.perms.Perm
import net.alasc.syntax.all._

case class ExtData(
  polytope: VPolytopeM[Rational], // TODO: use polytope without symmetry info
  rayRows: Set[Int],
  symmetryInfo: Option[SymmetryInfo] = None)

object ExtData {
  def fromNonSymPolytope(polytope: VPolytopeM[Rational]): ExtData = {
    val rayCols = (polytope.vertices.size until (polytope.vertices.size + polytope.rays.size)).toSet
    ExtData(polytope, rayCols)
  }

  def fromSymPolytope(polytope: VPolytopeM[Rational]): ExtData = { // TODO: support rays
    require(polytope.rays.isEmpty)
    val generators = polytope.symGroup.generators.toSeq
    val permutations = generators.map(_._1)
    val order = polytope.symGroup.order
    val symInfo = SymmetryInfo(false, Some(order), permutations, Seq.empty[Int])
    ExtData(polytope, Set.empty[Int], Some(symInfo))
  }

  implicit val FormatRead: FormatRead[ExtData] = new ExtDataRead
  implicit val FormatWrite: FormatWrite[ExtData] = new ExtDataWrite
}
