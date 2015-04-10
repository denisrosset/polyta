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

class ExtDataRead[M, V](implicit val M: MatVecInField[M, V, Rational]) extends FormatRead[ExtData[M, V]] {
  implicit def V: VecInField[V, Rational] = M.V

  object Parser extends ParserBase with SympolParserMV[M, V] {
    implicit def M: MatVecInField[M, V, Rational] = ExtDataRead.this.M

    def vector(d: Int): Parser[V] = repN(d, rational) ^^ { seq => V.build(seq: _*) }

    type VertexOrRay = Either[V, V]
    type Vertex = Left[V, V]
    type Ray = Right[V, V]

    def vertex(d: Int): Parser[Vertex] = "1" ~> vector(d) ^^ { v => Left[V, V](v) }
    def ray(d: Int): Parser[Ray] = "0" ~> vector(d) ^^ { v => Right[V, V](v) }

    def vertexOrRay(d: Int): Parser[VertexOrRay] = vertex(d) | ray(d)

    def vPolyhedron: Parser[(Boolean, VPolyhedron[M, V, Rational], Set[Int])] =
      (("V-representation" ~ lineEnding) ~> upToSymBeginLE ~ dimensions) into {
        case ~(upTo: Boolean, (m: Int, d: Int)) => (repN(m, vertexOrRay(d) <~ lineEnding) <~ ("end" ~ lineEnding)) ^^ { seq =>
          val rayRows = seq.zipWithIndex.collect {
            case (_: Ray, i) => i
          }.toSet
          val (verticesV, raysV) = util.PartitionEither(seq)
          val vertices = M.vertcat(M.zeros(0, d) +: verticesV.map(_.rowMat[M]): _*)
          val rays = M.vertcat(M.zeros(0, d) +: raysV.map(_.rowMat[M]): _*)
          (upTo, VPolyhedron(vertices, rays), rayRows)
        }
      }

    def data: Parser[ExtData[M, V]] = phrase(
      comments(HVHeader) ~> vPolyhedron into {
        case (upTo, poly, rayRows) => opt(symmetryInfo(upTo)) <~ opt(lineEndings) ^^ { symOption =>
          ExtData(poly, rayRows, symOption)
        }
      })
  }
}
