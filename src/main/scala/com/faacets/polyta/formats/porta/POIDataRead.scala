package com.faacets
package polyta
package formats
package porta


import spire.math.Rational

import scalin.immutable.Mat
import scalin.immutable.dense._
import scalin.syntax.all._

import fastparse.noApi._

final class POIDataRead extends FormatRead[POIData] with PortaDataParsers {

  def matrix(nCols: Int): P[Mat[Rational]] =
    (lineNumberForget.? ~ rowVector(nCols)).rep(sep = lineEndings).map { rows =>
      tabulate(rows.size, nCols)((r, c) => rows(r)(c))
    }

  sealed trait Section
  case class Conv(mV: Mat[Rational]) extends Section
  case class Cone(mR: Mat[Rational]) extends Section

  def coneSection(d: Int): P[Cone] =
    P( "CONE_SECTION" ~/ lineEndings ~ matrix(d) ).map(Cone(_))


  def convSection(d: Int): P[Conv] =
    P( "CONV_SECTION" ~/ lineEndings ~ matrix(d) ).map(Conv(_))

  def section(d: Int): P[Section] = coneSection(d) | convSection(d)

  def sections(d: Int): P[Seq[Section]] = (section(d) ~ lineEndings).rep

  def polytope(d: Int): P[VPolytope[Rational]] = sections(d)
    .filter(_.count(_.isInstanceOf[Cone]) <= 1)
    .filter(_.count(_.isInstanceOf[Conv]) <= 1)
    .map { sections =>
      val mR = sections.collect { case sec: Cone => sec.mR }.headOption.getOrElse(zeros[Rational](0, d))
      val mV = sections.collect { case sec: Conv => sec.mV }.headOption.getOrElse(zeros[Rational](0, d))
      VPolytope(mV, mR)
    }

  def data: P[POIData] = P(dimSection ~ lineEndings).flatMap { d => polytope(d) ~ end }.map(POIData(_))

}
