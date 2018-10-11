package com.faacets
package polyta
package formats
package porta

import java.io.Writer

import spire.math.Rational
import fastparse.noApi._
import scalin.immutable.Mat
import scalin.immutable.dense._
import spire.syntax.cfor.cforRange

case class POIData(polytope: VPolytope[Rational]) {

  def fileContents: String = {
    val sw = new java.io.StringWriter
    POIData.Write.write(this, sw)
    sw.toString
  }
}

object POIData {

  def parser: P[POIData] = Read.parser

  object Write {
    def writeDim(d: Int, out: Writer): Unit = {
      out.write("DIM = ")
      out.write(d.toString)
      out.write("\n\n")
    }

    def writeConv(vertices: Mat[Rational], out: Writer): Unit = {
      out.write("CONV_SECTION\n")
      cforRange(0 until vertices.nRows) { r =>
        StandardWriters.writeVectorSep[Rational](vertices(r, ::), " ", out)
        out.write("\n")
      }
      out.write("\n")
    }

    def writeCone(rays: Mat[Rational], out: Writer): Unit = {
      out.write("CONE_SECTION\n")
      cforRange(0 until rays.nRows) { r =>
        StandardWriters.writeVectorSep[Rational](rays(r, ::), " ", out)
        out.write("\n")
      }
      out.write("\n")
    }

    def writeEnd(out: Writer): Unit =
      out.write("END\n")

    def write(data: POIData, out: Writer): Unit = {
      writeDim(data.polytope.dim, out)
      if (data.polytope.mV.nRows > 0) writeConv(data.polytope.mV, out)
      if (data.polytope.mR.nRows > 0) writeCone(data.polytope.mR, out)
      writeEnd(out)
    }

  }

  object Read {

    import White._
    import StandardParsers._
    import PortaDataParsers._

    def matrix(nCols: Int): P[Mat[Rational]] =
      (lineNumberForget.? ~ rowVector(nCols)).rep(sep = lineEndings).map { rows =>
        Mat.tabulate(rows.size, nCols)((r, c) => rows(r)(c))
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
        val mR = sections.collect { case sec: Cone => sec.mR }.headOption.getOrElse(Mat.zeros[Rational](0, d))
        val mV = sections.collect { case sec: Conv => sec.mV }.headOption.getOrElse(Mat.zeros[Rational](0, d))
        VPolytope(mV, mR)
      }

    def parser: P[POIData] = P(dimSection ~ lineEndings).flatMap { d => polytope(d) ~ end }.map(POIData(_))
  }

}
