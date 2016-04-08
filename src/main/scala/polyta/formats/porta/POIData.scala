package com.faacets
package polyta
package formats
package porta

import spire.math.Rational

import scalin.{Mat, Vec}

case class POIData(polytope: VPolytopeM[Rational])

object POIData {

  implicit val FormatRead: FormatRead[POIData] = new POIDataRead

  implicit val FormatWrite: FormatWrite[POIData] = new POIDataWrite

}
