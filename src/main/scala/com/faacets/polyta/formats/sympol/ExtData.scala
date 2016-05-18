package com.faacets
package polyta
package formats
package sympol

import spire.math.Rational

case class ExtData(
  polytope: VPolytope[Rational],
  rayRows: Set[Int],
  symmetryInfo: Option[SymmetryInfo] = None)

object ExtData {

  def fromNonSymPolytope(polytope: VPolytope[Rational]): ExtData = {
    val rayCols = (polytope.mV.nRows until (polytope.mV.nRows + polytope.mR.nRows)).toSet
    ExtData(polytope, rayCols)
  }
/*
  def fromSymPolytope(polytope: VPolytopeCombSym[Rational]): ExtData = {
    val generators = polytope.symGroup.generators.toSeq
    val permutations = generators.map(_._1)
    val order = polytope.symGroup.order
    val symInfo = SymmetryInfo(false, Some(order), permutations, Seq.empty[Int])
    ExtData(polytope, Set.empty[Int], Some(symInfo))
  }*/

  implicit val formatRead: FormatRead[ExtData] = new ExtDataRead

  implicit val formatWrite: FormatWrite[ExtData] = new ExtDataWrite

}
