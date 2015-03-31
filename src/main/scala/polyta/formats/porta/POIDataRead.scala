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
  implicit def M: MatVecInField[M, V, Rational]
  implicit def V: VecInField[V, Rational] = M.V

  type VPoly = VPolyhedron[M, V, Rational]

  object Parser extends ParserBase with PortaDataParser[V] {
    implicit def V = self.V

    def matrix(nCols: Int): Parser[M] = repsep(opt(lineNumber) ~> rowVector(nCols), lineEndings) ^^ { rows =>
      M.fromFunM(new FunM[Rational] {
        def nR = rows.size
        def nC = nCols
        def f(r: Int, c: Int): Rational = rows(r)(c)
      })
    }

    def coneSection(d: Int): Parser[VPoly] =
      (("CONE_SECTION" ~ lineEndings) ~> matrix(d)).map { m =>
        VPolyhedron.fromRays[M, V, Rational](m)
      }

    def convSection(d: Int): Parser[VPoly] =
      (("CONV_SECTION" ~ lineEndings) ~> matrix(d)).map { m =>
        VPolyhedron.fromVertices[M, V, Rational](m)
      }

    def polyhedronSection(d: Int): Parser[VPoly] = coneSection(d) | convSection(d)

    def polyhedron(d: Int): Parser[VPoly] = rep(polyhedronSection(d) <~ lineEndings) ^^ { seq =>
      (seq.head /: seq.tail) {
        (p1, p2) =>
        VPolyhedron[M, V, Rational](
          vertcat(p1.vertices, p2.vertices),
          vertcat(p1.rays, p2.rays)
        )
      }
    }

    def data: Parser[POIData[M, V]] = (dimSection <~ lineEndings) into { d =>
      polyhedron(d) <~ end ^^ { polyhedron => POIData(polyhedron) }
    }
  }
}
