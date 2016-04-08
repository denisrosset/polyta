package com.faacets
package polyta
package formats
package panda

import scala.util.parsing.combinator._

import spire.math.Rational

case class VData(
  polytope: VPolytopeM[Rational],
  names: Option[Seq[String]] = None,
  maps: Seq[AffineTransform[Rational]] = Seq.empty)

object VData {

  implicit val FormatRead: FormatRead[VData] = new VDataRead

  implicit val FormatWrite: FormatWrite[VData] = new VDataWrite

}
