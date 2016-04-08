package com.faacets
package polyta
package formats
package porta

import java.io.{Reader, Writer}

import scala.{specialized => sp}

import scala.collection.{BitSet, SortedSet}

import spire.algebra._
import spire.math.Rational
import spire.syntax.order._
import spire.syntax.vectorSpace._
import spire.syntax.cfor._
import spire.util._

import scalin.{Mat, Vec}
import scalin.immutable.dense._

final class POIDataWrite extends FormatWrite[POIData] {

  def writeDim(d: Int, out: Writer): Unit = {
    out.write("DIM = ")
    out.write(d.toString)
    out.write("\n\n")
  }

  def writeConv(vertices: Mat[Rational], out: Writer): Unit = {
    out.write("CONV_SECTION\n")
    cforRange(0 until vertices.nCols) { c =>
      Format.writeVectorSep[Rational](vertices(::, c), " ", out)
      out.write("\n")
    }
    out.write("\n")
  }

  def writeCone(rays: Mat[Rational], out: Writer): Unit = {
    out.write("CONE_SECTION\n")
    cforRange(0 until rays.nCols) { c =>
      Format.writeVectorSep[Rational](rays(::, c), " ", out)
      out.write("\n")
    }
    out.write("\n")
  }

  def writeEnd(out: Writer): Unit =
    out.write("END\n")

  def write(data: POIData, out: Writer): Unit = {
    writeDim(data.polytope.dim, out)
    if (data.polytope.vertices.nonEmpty) writeConv(data.polytope.mV, out)
    if (data.polytope.rays.nonEmpty) writeCone(data.polytope.mR, out)
    writeEnd(out)
  }

}
