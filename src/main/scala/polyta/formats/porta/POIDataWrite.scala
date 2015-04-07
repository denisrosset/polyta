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

trait POIDataWrite[M, V] extends Any with FormatWrite[POIData[M, V]] {
  implicit def M: MatVecInField[M, V, Rational]
  implicit def V: VecInField[V, Rational] = M.V

  def writeDim(d: Int, out: Writer): Unit = {
    out.write("DIM = ")
    out.write(d.toString)
    out.write("\n\n")
  }

  def writeConv(vertices: M, out: Writer): Unit = {
    out.write("CONV_SECTION\n")
    cforRange(0 until vertices.nRows) { r =>
      Format.writeVectorSep[FunV[Rational], Rational](vertices.view(r, ::), " ", out)
    }
    out.write("\n")
  }

  def writeCone(rays: M, out: Writer): Unit = {
    out.write("CONE_SECTION\n")
    cforRange(0 until rays.nRows) { r =>
      Format.writeVectorSep[FunV[Rational], Rational](rays.view(r, ::), " ", out)
    }
    out.write("\n")
  }

  def writeEnd(out: Writer): Unit =
    out.write("END\n")

  def write(data: POIData[M, V], out: Writer): Unit = {
    writeDim(data.polyhedron.nX, out)
    if (data.polyhedron.nVertices > 0) writeConv(data.polyhedron.vertices, out)
    if (data.polyhedron.nRays > 0) writeCone(data.polyhedron.rays, out)
    writeEnd(out)
  }
}