package com.faacets
package polyta
package formats
package porta


import spire.math.Rational

import scalin.immutable.dense._
import scalin.immutable.{DenseMat => IMat}
import scalin.syntax.all._

final class POIDataRead extends FormatRead[POIData] { self =>

  object Parsers extends ParsersBase with PortaDataParsers {

    def matrix(nCols: Int): Parser[IMat[Rational]] = repsep(opt(lineNumber) ~> rowVector(nCols), lineEndings) ^^ { rows =>
      IMat.tabulate(rows.size, nCols)( (r, c) => rows(r)(c))
    }

    def coneSection(d: Int): Parser[VPolytopeM[Rational]] =
      (("CONE_SECTION" ~ lineEndings) ~> matrix(d)).map { m =>
        VPolytopeM.fromRays[Rational](m)
      }

    def convSection(d: Int): Parser[VPolytopeM[Rational]] =
      (("CONV_SECTION" ~ lineEndings) ~> matrix(d)).map { m =>
        VPolytopeM.fromVertices[Rational](m)
      }

    def polytopeSection(d: Int): Parser[VPolytopeM[Rational]] = coneSection(d) | convSection(d)

    def polytope(d: Int): Parser[VPolytopeM[Rational]] = rep(polytopeSection(d) <~ lineEndings) ^^ { seq =>
      (seq.head /: seq.tail) {
        (p1, p2) =>
        VPolytopeM(
          colMat(p1.mV, p2.mV).flatten,
          colMat(p1.mR, p2.mR).flatten
        )
      }
    }

    def data: Parser[POIData] = (dimSection <~ lineEndings) into { d =>
      polytope(d) <~ end ^^ { polytope => POIData(polytope) }
    }

  }

}
