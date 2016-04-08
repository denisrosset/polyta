package com.faacets
package polyta
package formats
package panda

import spire.math.Rational

case class HData(
  polytope: HPolytopeM[Rational],
  names: Option[Seq[String]] = None,
  maps: Seq[AffineTransform[Rational]] = Seq.empty)

object HData {

  implicit val FormatRead: FormatRead[HData] = new HDataRead

  implicit val FormatWrite: FormatWrite[HData] = new HDataWrite

}
