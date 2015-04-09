package com.faacets
package polyta
package formats
package sympol

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

import net.alasc.math._
import net.alasc.syntax.all._

class ExtDataWrite[M, V](implicit val M: MatVecInField[M, V, Rational]) extends FormatWrite[ExtData[M, V]] with SympolDataWrite[M, V] {

  def writeHeader(upToSymmetry: Boolean, out: Writer): Unit = {
    out.write("V-representation\n")
    if (upToSymmetry) out.write("* UP TO SYMMETRY\n")
  }

  def writePolyhedron(poly: VPolyhedron[M, V, Rational], rayRows: Set[Int], out: Writer): Unit = {
    out.write("begin\n")
    val n = poly.nVertices + poly.nRays
    require(rayRows.size == poly.nRays)
    out.write(n.toString)
    out.write(" ")
    out.write((poly.nX + 1).toString)
    out.write(" rational\n")
    var vertR = 0
    var rayR = 0
    cforRange(0 until n) { r =>
      if (rayRows.contains(r)) {
        out.write("0 ")
        Format.writeVectorSep[V, Rational](poly.mR(rayR, ::), " ", out)
        rayR += 1
      } else {
        out.write("1 ")
        Format.writeVectorSep[V, Rational](poly.mV(vertR, ::), " ", out)
        vertR += 1
      }
      out.write("\n")
    }
    out.write("end\n")
  }

  def write(data: ExtData[M, V], out: Writer): Unit = {
    writeHeader(data.symmetryInfo.fold(false)(_.upToSymmetryWRTO), out)
    writePolyhedron(data.polyhedron, data.rayRows, out)
    data.symmetryInfo.foreach { writeSymmetryInfo(_, out) }
  }
}
