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

  object Parser extends ParserBase with SympolParserMV[M, V] {
    implicit def M: MatVecInField[M, V, Rational] = IneDataRead.this.M

    def linearity: Parser[Seq[Int]] = ("linearity" ~> positiveInt) into { n =>
      repN(n, positiveInt) ^^ { seq => seq.map(_ - 1) }
    }

    def hPolyhedron: Parser[(Boolean, HPolyhedron[M, V, Rational], Set[Int])] =
      (("H-representation" ~ lineEnding) ~> upToSymBeginLE ~ opt(linearity) ~ dimensions) into {
        case ~(~(upTo: Boolean, linOpt), (m: Int, d: Int)) => matrix(m, d + 1) <~ ("end" ~ lineEnding) ^^ { mat =>
          val equalityRows = linOpt.getOrElse(Seq.empty).sorted
          val inequalityRows = ((0 until m).toSet -- equalityRows.toSet).toSeq.sorted
          val mAeq = -mat(equalityRows, 1 to d)
          val vbeq = mat(equalityRows, 0)
          val mA = -mat(inequalityRows, 1 to d)
          val vb = mat(inequalityRows, 0)
          (upTo, HPolyhedron(mA, vb, mAeq, vbeq), equalityRows.toSet)
        }
      }

    def data: Parser[IneData[M, V]] = phrase(comments(HVHeader) ~> hPolyhedron into {
      case (upTo, poly, equalityRows) => opt(symmetryInfo(upTo)) <~ opt(lineEndings) ^^ { symOption =>
        IneData(poly, equalityRows, symOption)
      }
    })
  }
}
