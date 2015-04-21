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

import qalg.algebra._
import qalg.algos._
import qalg.math._
import qalg.syntax.all._

final class POIDataWrite[M, V](implicit val M: MatVecInField[M, V, Rational]) extends FormatWrite[POIData[M, V]] {
  implicit def V: VecInField[V, Rational] = M.V

  def writeDim(d: Int, out: Writer): Unit = {
    out.write("DIM = ")
    out.write(d.toString)
    out.write("\n\n")
  }

  def writeConv(vertices: M, out: Writer): Unit = {
    out.write("CONV_SECTION\n")
    cforRange(0 until vertices.nCols) { c =>
      Format.writeVectorSep[FunV[Rational], Rational](vertices.view(::, c), " ", out)
      out.write("\n")
    }
    out.write("\n")
  }

  def writeCone(rays: M, out: Writer): Unit = {
    out.write("CONE_SECTION\n")
    cforRange(0 until rays.nCols) { c =>
      Format.writeVectorSep[FunV[Rational], Rational](rays.view(::, c), " ", out)
      out.write("\n")
    }
    out.write("\n")
  }

  def writeEnd(out: Writer): Unit =
    out.write("END\n")

  def write(data: POIData[M, V], out: Writer): Unit = {
    writeDim(data.polyhedron.nX, out)
    if (data.polyhedron.vertices.nonEmpty) writeConv(data.polyhedron.mV, out)
    if (data.polyhedron.rays.nonEmpty) writeCone(data.polyhedron.mR, out)
    writeEnd(out)
  }
}
