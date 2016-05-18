package com.faacets
package polyta
package formats
package porta

import spire.math.Rational

case class POIData(polytope: VPolytope[Rational])

object POIData {

  implicit val formatRead: FormatRead[POIData] = new POIDataRead

  implicit val formatWrite: FormatWrite[POIData] = new POIDataWrite

}
