package com.faacets
package polyta
package formats
package sympol

import spire.math.Rational

import net.alasc.perms.Perm
import net.alasc.syntax.all._

case class IneData(
  polytope: HPolytopeM[Rational], // TODO: use polytope without symmetry info
  equalityRows: Set[Int],
  symmetryInfo: Option[SymmetryInfo] = None)

object IneData {

  def fromNonSymPolytope(polytope: HPolytopeM[Rational]): IneData = {
    val equalityRows = (polytope.facets.size until (polytope.facets.size + polytope.equalities.size)).toSet
    IneData(polytope, equalityRows)
  }

  def fromSymPolytope(polytope: HPolytopeM[Rational]): IneData = {
    val equalityRows = (polytope.facets.size until (polytope.facets.size + polytope.equalities.size)).toSet
    val generators = polytope.symGroup.generators.toSeq
    val permutations = generators.map(_.to[Perm])
    val order = polytope.symGroup.order
    val symInfo = SymmetryInfo(false, Some(order), permutations, Seq.empty[Int])
    IneData(polytope, equalityRows, Some(symInfo))
  }

  implicit val FormatRead: FormatRead[IneData] = new IneDataRead

  implicit val FormatWrite: FormatWrite[IneData] = new IneDataWrite

}
