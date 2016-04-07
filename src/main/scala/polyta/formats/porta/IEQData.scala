package com.faacets
package polyta
package formats
package porta

import spire.math.Rational

import scalin.{Mat, Vec}

case class IEQData(
  polytope: HPolytopeCombSym[Rational],
  validPoint: Option[Vec[Rational]] = None,
  eliminationOrder: Option[Seq[Int]] = None,
  lowerBounds: Option[Vec[Rational]] = None,
  upperBounds: Option[Vec[Rational]] = None
)

object IEQData {
  def empty(dim: Int)(implicit A: LinAlg[Rational]): IEQData = IEQData(HPolytopeCombSym.empty[Rational](dim))
  /* TODO
  implicit def FormatRead[M, V](implicit alg: AlgMVF[M, V, Rational]): FormatRead[IEQData[M, V]] = new IEQDataRead[M, V]
  implicit def FormatWrite[M, V](implicit alg: AlgMVF[M, V, Rational]): FormatWrite[IEQData[M, V]] = new IEQDataWrite[M, V]
   */
}
