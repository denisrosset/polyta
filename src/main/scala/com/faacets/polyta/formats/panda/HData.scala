package com.faacets
package polyta
package formats
package panda

import spire.math.Rational

case class HData(
  polytope: HPolytope[Rational],
  names: Option[Seq[String]] = None,
  maps: Seq[AffineTransform[Rational]] = Seq.empty)

object HData {

  implicit val formatRead: FormatRead[HData] = new HDataRead

  implicit val formatWrite: FormatWrite[HData] = new HDataWrite

}
