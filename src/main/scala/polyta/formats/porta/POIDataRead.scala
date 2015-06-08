package com.faacets
package polyta
package formats
package porta

import java.io.{Reader, Writer}

import scala.{specialized => sp}

import scala.collection.{BitSet, SortedSet}

import spire.algebra._
import spire.math.Rational
import spire.syntax.order._
import spire.syntax.vectorSpace._
import spire.syntax.cfor._
import spire.util._

import qalg.algebra._
import qalg.algos._
import qalg.math._
import qalg.syntax.all._

trait POIDataRead[M, V] extends FormatRead[POIData[M, V]] { self =>
  implicit def alg: AlgMVF[M, V, Rational]

  type VPoly = VPolyhedronM[M, V, Rational]

  object Parsers extends ParsersBase with PortaDataParsers[V] {
    implicit def alg = POIDataRead.this.alg

    def matrix(nCols: Int): Parser[M] = repsep(opt(lineNumber) ~> rowVector(nCols), lineEndings) ^^ { rows =>
      MatBuilder[M, Rational].tabulate(rows.size, nCols)( (r, c) => rows(r)(c))
    }

    def coneSection(d: Int): Parser[VPoly] =
      (("CONE_SECTION" ~ lineEndings) ~> matrix(d)).map { m =>
        VPolyhedronM.fromRays[M, V, Rational](m.t)
      }

    def convSection(d: Int): Parser[VPoly] =
      (("CONV_SECTION" ~ lineEndings) ~> matrix(d)).map { m =>
        VPolyhedronM.fromVertices[M, V, Rational](m.t)
      }

    def polyhedronSection(d: Int): Parser[VPoly] = coneSection(d) | convSection(d)

    def polyhedron(d: Int): Parser[VPoly] = rep(polyhedronSection(d) <~ lineEndings) ^^ { seq =>
      (seq.head /: seq.tail) {
        (p1, p2) =>
        VPolyhedronM.apply[M, V, Rational](
          horzcat(p1.mV, p2.mV),
          horzcat(p1.mR, p2.mR)
        )
      }
    }

    def data: Parser[POIData[M, V]] = (dimSection <~ lineEndings) into { d =>
      polyhedron(d) <~ end ^^ { polyhedron => POIData(polyhedron) }
    }
  }
}
