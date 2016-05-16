package com.faacets
package polyta
package formats
package sympol

import spire.math.Rational

case class IneData(
  polytope: HPolytope[Rational],
  equalityRows: Set[Int],
  symmetryInfo: Option[SymmetryInfo] = None)

object IneData {

  def fromHPolytope(polytope: HPolytope[Rational]): IneData = {
    val equalityRows = (polytope.mA.nRows until (polytope.mA.nRows + polytope.mAeq.nRows)).toSet
    IneData(polytope, equalityRows)
  }
/*
  def fromHPolytopeCombSym(polytope: HPolytopeCombSym[Rational]): IneData = {
    val equalityRows = (polytope.facets.size until (polytope.facets.size + polytope.equalities.size)).toSet
    val generators = polytope.symGroup.generators.toSeq
    val permutations = generators.map(_.to[Perm])
    val order = polytope.symGroup.order
    val symInfo = SymmetryInfo(false, Some(order), permutations, Seq.empty[Int])
    IneData(polytope, equalityRows, Some(symInfo))
  }*/

  implicit val formatRead: FormatRead[IneData] = new IneDataRead

  implicit val formatWrite: FormatWrite[IneData] = new IneDataWrite

}
