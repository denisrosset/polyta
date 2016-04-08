package com.faacets
package polyta
package formats
package sympol

import scala.language.implicitConversions

import scala.util.parsing.combinator._

import spire.math.Rational
import spire.syntax.field._

import scalin.immutable.dense._

import ComparisonOp.LE

class IneDataRead extends FormatRead[IneData] {

  object Parsers extends ParsersBase with SympolParsers {

    def linearity: Parser[Seq[Int]] = ("linearity" ~> positiveInt) into { n =>
      repN(n, positiveInt) ^^ { seq => seq.map(_ - 1) }
    }

    def hPolytope: Parser[(Boolean, HPolytopeM[Rational], Set[Int])] =
      (("H-representation" ~ lineEnding) ~> upToSymLE ~ opt(linearity <~ lineEnding) ~ (("begin" ~ lineEnding) ~> dimensions)) into {
        case ~(~(upTo: Boolean, linOpt), (m: Int, d: Int)) => repN(m, rowVector(d + 1) <~ lineEnding) <~ ("end" ~ lineEnding) ^^ { rowVectors =>
          val equalityRows = linOpt.getOrElse(Seq.empty).sorted
          val inequalityRows = ((0 until m).toSet -- equalityRows.toSet).toSeq.sorted
          val equalities = equalityRows.map { k =>
            val row = rowVectors(k)
            LinearEquality(-row(1 to d), row(0))
          }
          val inequalities = inequalityRows.map { k =>
            val row = rowVectors(k)
            LinearInequality(-row(1 to d), LE, row(0))
          }
          (upTo, HPolytopeM(d, inequalities, equalities), equalityRows.toSet)
        }
      }

    def data: Parser[IneData] = phrase(comments(HVHeader) ~> hPolytope into {
      case (upTo, poly, equalityRows) => opt(symmetryInfo(upTo)) <~ opt(lineEndings) ^^ { symOption =>
        IneData(poly, equalityRows, symOption)
      }
    })
  }
}
