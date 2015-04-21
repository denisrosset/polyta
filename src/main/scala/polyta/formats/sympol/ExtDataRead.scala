package com.faacets
package polyta
package formats
package sympol

import scala.util.parsing.combinator._

import spire.math.Rational
import spire.syntax.field._

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

class ExtDataRead[V](implicit val V: VecInField[V, Rational]) extends FormatRead[ExtData[V]] {
  type VPoly = VPolyhedron[V, Rational]

  object Parser extends ParserBase with SympolParserV[V] {
    implicit def V: VecInField[V, Rational] = ExtDataRead.this.V

    def vector(d: Int): Parser[V] = repN(d, rational) ^^ { seq => V.build(seq: _*) }

    type VertexOrRay = Either[V, V]
    type Vertex = Left[V, V]
    type Ray = Right[V, V]

    def vertex(d: Int): Parser[Vertex] = "1" ~> vector(d) ^^ { v => Left[V, V](v) }
    def ray(d: Int): Parser[Ray] = "0" ~> vector(d) ^^ { v => Right[V, V](v) }

    def vertexOrRay(d: Int): Parser[VertexOrRay] = vertex(d) | ray(d)

    def vPolyhedron: Parser[(Boolean, VPoly, Set[Int])] =
      (("V-representation" ~ lineEnding) ~> upToSymBeginLE ~ dimensions) into {
        case ~(upTo: Boolean, (m: Int, d: Int)) => (repN(m, vertexOrRay(d) <~ lineEnding) <~ ("end" ~ lineEnding)) ^^ { seq =>
          val rayCols = seq.zipWithIndex.collect {
            case (_: Ray, i) => i
          }.toSet
          val (verticesV, raysV) = util.PartitionEither(seq)
          (upTo, VPolyhedron.apply(verticesV, raysV), rayCols)
        }
      }

    def data: Parser[ExtData[V]] = phrase(
      comments(HVHeader) ~> vPolyhedron into {
        case (upTo, poly, rayCols) => opt(symmetryInfo(upTo)) <~ opt(lineEndings) ^^ { symOption =>
          ExtData(poly, rayCols, symOption)
        }
      })
  }
}
