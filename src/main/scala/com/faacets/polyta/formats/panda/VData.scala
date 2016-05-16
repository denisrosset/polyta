package com.faacets
package polyta
package formats
package panda

import spire.math.Rational

case class VData(
  polytope: VPolytope[Rational],
  names: Option[Seq[String]] = None,
  maps: Seq[AffineTransform[Rational]] = Seq.empty)

object VData {

  implicit val formatRead: FormatRead[VData] = new VDataRead

  implicit val formatWrite: FormatWrite[VData] = new VDataWrite

}
