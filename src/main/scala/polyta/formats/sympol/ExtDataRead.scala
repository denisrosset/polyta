package com.faacets
package polyta
package formats
package sympol

import scala.util.parsing.combinator._

import spire.math.Rational
import spire.syntax.field._

import scalin.immutable.dense._
import scalin.immutable.{Mat => IMat, Vec => IVec}
import scalin.syntax.all._

class ExtDataRead extends FormatRead[ExtData] {

  type VPoly = VPolytopeM[Rational]

  object Parsers extends ParsersBase with SympolParsers {

    def vector(d: Int): Parser[IVec[Rational]] = repN(d, rational) ^^ { seq => vec(seq: _*) }

    type VertexOrRay = Either[IVec[Rational], IVec[Rational]]
    type Vertex = Left[IVec[Rational], IVec[Rational]]
    type Ray = Right[IVec[Rational], IVec[Rational]]

    def vertex(d: Int): Parser[Vertex] = "1" ~> vector(d) ^^ { v => Left[IVec[Rational], IVec[Rational]](v) }
    def ray(d: Int): Parser[Ray] = "0" ~> vector(d) ^^ { v => Right[IVec[Rational], IVec[Rational]](v) }

    def vertexOrRay(d: Int): Parser[VertexOrRay] = vertex(d) | ray(d)

    def vPolytope: Parser[(Boolean, VPoly, Set[Int])] =
      (("V-representation" ~ lineEnding) ~> upToSymBeginLE ~ dimensions) into {
        case ~(upTo: Boolean, (m: Int, d: Int)) => (repN(m, vertexOrRay(d) <~ lineEnding) <~ ("end" ~ lineEnding)) ^^ { seq =>
          val rayRows = seq.zipWithIndex.collect {
            case (_: Ray, i) => i
          }.toSet
          val (verticesV, raysV) = util.PartitionEither(seq)
          (upTo, VPolytopeM[Rational](d, verticesV, raysV), rayRows)
        }
      }

    def data: Parser[ExtData] = phrase(
      comments(HVHeader) ~> vPolytope into {
        case (upTo, poly, rayRows) => opt(symmetryInfo(upTo)) <~ opt(lineEndings) ^^ { symOption =>
          ExtData(poly, rayRows, symOption)
        }
      })

  }

}
