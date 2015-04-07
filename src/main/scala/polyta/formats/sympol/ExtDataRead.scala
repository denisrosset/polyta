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

  object Parser extends ParserBase with SympolParser[M, V] {
    implicit def M: MatVecInField[M, V, Rational] = ExtDataRead.this.M

    def vector(d: Int): Parser[V] = repN(d, rational) ^^ { seq => V.build(seq: _*) }
    def vertex(d: Int): Parser[V] = "1" ~> vector(d)
    def ray(d: Int): Parser[V] = "0" ~> vector(d)

    def vertices(d: Int): Parser[M] = rep(vertex(d) <~ lineEnding) ^^ { seq =>
      M.vertcat(M.zeros(0, d) +: seq.map(_.rowMat[M]): _*)
    }

    def rays(d: Int): Parser[M] = rep(ray(d) <~ lineEnding) ^^ { seq =>
      M.vertcat(M.zeros(0, d) +: seq.map(_.rowMat[M]): _*)
    }


    def vPolyhedron: Parser[VPolyhedron[M, V, Rational]] =
      (("V-representation" ~ lineEnding ~ "begin" ~ lineEnding) ~> dimensions) into {
        case (m: Int, d: Int) => (vertices(d) ~ rays(d) <~ ("end" ~ lineEnding)) into {
          case vertices ~ rays if vertices.nRows + rays.nRows == m =>
            success(VPolyhedron(vertices, rays))
          case _ =>
            failure("Wrong number of vertices and rays in file")
        }
      }

    def data: Parser[ExtData[M, V]] = phrase((comments ~> (vPolyhedron ~ opt(symmetryInfo))) <~ opt(lineEndings)) ^^ {
      case poly ~ symOption => ExtData(poly, symOption)
    }
  }
}
