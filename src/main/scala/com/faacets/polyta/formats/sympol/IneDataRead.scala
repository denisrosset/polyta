package com.faacets
package polyta
package formats
package sympol

import spire.math.Rational

import scalin.immutable.dense._

import scalin.syntax.all._

import fastparse.noApi._

class IneDataRead extends FormatRead[IneData] with SympolParsers {

    def linearity: P[Seq[Int]] = P( "linearity" ~ positiveInt ).flatMap { n =>
      (Pass ~ positiveInt).rep(min=n, max=n).map( seq => seq.map(_ - 1) )
    }

    def hPolytope: P[(Boolean, HPolytope[Rational], Set[Int])] =
      P( "H-representation" ~ lineEnding ~ upToSymLE ~
        (linearity ~ lineEnding).? ~ "begin" ~ lineEnding ~ dimensions ).flatMap {
        case (upTo: Boolean, linOpt, (m: Int, d: Int)) =>
          ((Pass ~ rowVector(d + 1) ~ lineEnding).rep(min=m, max=m) ~ "end" ~ lineEnding).map { rowVectors =>
          val equalityRows = linOpt.getOrElse(Seq.empty).sorted
          val inequalityRows = ((0 until m).toSet -- equalityRows.toSet).toSeq.sorted
          val mAeq = tabulate(equalityRows.size, d) { (r, c) => -rowVectors(equalityRows(r))(c + 1) }
          val vbeq = tabulate(equalityRows.size) { k => rowVectors(equalityRows(k))(0) }
          val mA = tabulate(inequalityRows.size, d) { (r, c) => -rowVectors(inequalityRows(r))(c + 1) }
          val vb = tabulate(inequalityRows.size) { k => rowVectors(inequalityRows(k))(0) }
          (upTo, HPolytope(mA, vb, mAeq, vbeq), equalityRows.toSet)
        }
      }

    def data: P[IneData] = (comments(HVHeader) ~ hPolytope).flatMap {
      case (upTo, poly, equalityRows) => (Pass ~ symmetryInfo(upTo).? ~ lineEndings.? ~ End).map { symOption =>
        IneData(poly, equalityRows, symOption)
      }
    }

}
