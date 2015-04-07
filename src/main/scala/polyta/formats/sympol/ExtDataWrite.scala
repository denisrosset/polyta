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

  def writeHeader(out: Writer): Unit = {
    out.write("V-representation\n")
    out.write("begin\n")
  }

  def writeDimension(d: Int, nVertices: Int, nRays: Int, out: Writer): Unit = {
    out.write((nVertices + nRays).toString)
    out.write(" ")
    out.write((d + 1).toString)
    out.write(" rational\n")
  }

  def writeVertices(vertices: M, out: Writer): Unit = {
    cforRange(0 until vertices.nRows) { r =>
      out.write("1 ")
      Format.writeVectorSep[V, Rational](vertices(r, ::), " ", out)
      out.write("\n")
    }
  }

  def writeRays(rays: M, out: Writer): Unit = {
    cforRange(0 until rays.nRows) { r =>
      out.write("0 ")
      Format.writeVectorSep[V, Rational](rays(r, ::), " ", out)
      out.write("\n")
    }
  }

  def writePolyhedron(poly: VPolyhedron[M, V, Rational], out: Writer): Unit = {
    writeDimension(poly.nX, poly.nVertices, poly.nRays, out)
    writeVertices(poly.vertices, out)
    writeRays(poly.rays, out)
    out.write("end\n")
  }

  def write(data: ExtData[M, V], out: Writer): Unit = {
    writeHeader(out)
    writePolyhedron(data.polyhedron, out)
    data.symmetryInfo.foreach( writeSymmetryInfo(_, out) )
  }
}
