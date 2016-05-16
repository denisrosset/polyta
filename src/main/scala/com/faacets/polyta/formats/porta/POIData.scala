package com.faacets
package polyta
package formats
package porta

import spire.math.Rational

case class POIData(polytope: VPolytope[Rational])

object POIData {

  implicit val FormatRead: FormatRead[POIData] = new POIDataRead

  implicit val FormatWrite: FormatWrite[POIData] = new POIDataWrite

}
