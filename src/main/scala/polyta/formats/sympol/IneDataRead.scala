package com.faacets
package polyta
package formats
package sympol

import scala.util.parsing.combinator._

import spire.math.Rational
import spire.syntax.field._

import qalg.algebra._
import qalg.syntax.all._

class IneDataRead[M, V](implicit val M: MatVecInField[M, V, Rational]) extends FormatRead[IneData[M, V]] {
  implicit def V: VecInField[V, Rational] = M.V

  object Parser extends ParserBase with SympolParser[M, V] {
    implicit def M: MatVecInField[M, V, Rational] = IneDataRead.this.M

    def hPolyhedronMatrix: Parser[M] =
      (("H-representation" ~ lineEnding ~ "begin" ~ lineEnding) ~> dimensions) into {
        case (m: Int, d: Int) => matrix(m, d + 1) <~ ("end" ~ lineEnding)
      }

    def hPolyhedron: Parser[HFullPolyhedron[M, V, Rational]] = hPolyhedronMatrix.map { m =>
      val mA = m(::, 1 until m.nCols)
      val vb = m(::, 0)
      HFullPolyhedron(mA, vb)
    }

    def data: Parser[IneData[M, V]] = phrase((comments ~> (hPolyhedron ~ opt(symmetryInfo))) <~ opt(lineEndings)) ^^ {
      case poly ~ symOption => IneData(poly, symOption)
    }
  }
}
