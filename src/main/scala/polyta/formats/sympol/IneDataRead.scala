package com.faacets
package polyta
package formats
package sympol

import scala.language.implicitConversions

import scala.util.parsing.combinator._

import spire.math.Rational
import spire.syntax.field._

import qalg.indup.algebra._
import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

class IneDataRead[V](implicit val pack: PackField.ForV[V, Rational]) extends FormatRead[IneData[V]] {

  type HPoly = HPolytope[V, Rational]

  object Parsers extends ParsersBase with SympolParsersV[V] {

    implicit def pack: PackField.ForV[V, Rational] = IneDataRead.this.pack

    def linearity: Parser[Seq[Int]] = ("linearity" ~> positiveInt) into { n =>
      repN(n, positiveInt) ^^ { seq => seq.map(_ - 1) }
    }

    def hPolyhedron: Parser[(Boolean, HPoly, Set[Int])] =
      (("H-representation" ~ lineEnding) ~> upToSymBeginLE ~ opt(linearity) ~ dimensions) into {
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
          (upTo, HPolytope(d, inequalities, equalities), equalityRows.toSet)
        }
      }

    def data: Parser[IneData[V]] = phrase(comments(HVHeader) ~> hPolyhedron into {
      case (upTo, poly, equalityRows) => opt(symmetryInfo(upTo)) <~ opt(lineEndings) ^^ { symOption =>
        IneData(poly, equalityRows, symOption)
      }
    })
  }
}
