package com.faacets
package polyta
package formats
package sympol

import spire.math.Rational

import scalin.immutable.dense._
import scalin.immutable.Vec
import scalin.syntax.all._

import fastparse.noApi._

class ExtDataRead extends FormatRead[ExtData] with SympolParsers {

  type VPoly = VPolytope[Rational]

  def vector(d: Int): P[Vec[Rational]] = rational.rep(min=d, max=d).map( seq => vec(seq: _*) )

  case class Element(vec: Vec[Rational], isVertex: Boolean)

  def vertex(d: Int): P[Element] = P( "1" ~ vector(d) ).map( v => Element(v, true) )

  def ray(d: Int): P[Element] = P( "0" ~ vector(d) ).map( v => Element(v, false) )

  def vertexOrRay(d: Int): P[Element] = vertex(d) | ray(d)

  def vPolytope: P[(Boolean, VPoly, Set[Int])] =
    P( "V-representation" ~ lineEnding ~ upToSymBeginLE ~ dimensions ).flatMap {
      case (upTo: Boolean, (m: Int, d: Int)) =>
        ((Pass ~ vertexOrRay(d) ~ lineEnding).rep(min=m, max=m) ~ "end" ~ lineEnding).map { seq =>
          val rayRows = seq.zipWithIndex.collect {
            case (Element(_, false), i) => i
          }.sorted
          val vertexRows = seq.zipWithIndex.collect {
            case (Element(_, true), i) => i
          }.sorted
          val mR = tabulate(rayRows.size, d) { (r, c) => seq(rayRows(r)).vec(c) }
          val mV = tabulate(vertexRows.size, d) { (r, c) => seq(vertexRows(r)).vec(c) }
          (upTo, VPolytope[Rational](mV, mR), rayRows.toSet)
        }
    }

  def data: P[ExtData] = (comments(HVHeader) ~ vPolytope).flatMap {
    case (upTo, poly, rayRows) => (symmetryInfo(upTo).? ~ lineEndings.?).map { symOption =>
      ExtData(poly, rayRows, symOption)
    }
  }

}
