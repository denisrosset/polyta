package com.faacets
package polyta
package formats
package porta

import spire.math.Rational

import scalin.{Mat, Vec}

case class IEQData(
  polytope: HPolytopeM[Rational],
  validPoint: Option[Vec[Rational]] = None,
  eliminationOrder: Option[Seq[Int]] = None,
  lowerBounds: Option[Vec[Rational]] = None,
  upperBounds: Option[Vec[Rational]] = None
)

object IEQData {
  
  def empty(dim: Int)(implicit A: LinAlg[Rational]): IEQData = IEQData(HPolytopeM.empty[Rational](dim))

  implicit val FormatRead: FormatRead[IEQData] = new IEQDataRead

  implicit val FormatWrite: FormatWrite[IEQData] = new IEQDataWrite

}
