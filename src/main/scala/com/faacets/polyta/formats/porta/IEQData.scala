package com.faacets.polyta
package formats
package porta

import spire.math.Rational

import scalin.immutable.Vec

case class IEQData(
  polytope: HPolytope[Rational],
  validPoint: Vec[Rational],
  eliminationOrder: Option[Seq[Int]] = None,
  lowerBounds: Option[Vec[Rational]] = None,
  upperBounds: Option[Vec[Rational]] = None
)

object IEQData {
  
  implicit val formatRead: FormatRead[IEQData] = new IEQDataRead

  implicit val formatWrite: FormatWrite[IEQData] = new IEQDataWrite

}
