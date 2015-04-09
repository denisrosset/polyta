package com.faacets
package polyta
package formats
package panda

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

class VDataWrite[M, V](implicit val M: MatVecInField[M, V, Rational]) extends FormatWrite[VData[M, V]] with PandaDataWrite {
  import M.V

  def writeMatrix(m: M, out: Writer): Unit = {
    cforRange(0 until m.nRows) { r =>
      Format.writeVectorSep[V, Rational](m(r, ::), " ", out)
      out.write("\n")
    }
  }

  def writeVertices(vertices: M, out: Writer): Unit = {
    out.write("Vertices:\n")
    writeMatrix(vertices, out)
  }

  def writeRays(rays: M, out: Writer): Unit = {
    out.write("Rays:\n")
    writeMatrix(rays, out)
  }

  def writePolyhedron(poly: VPolyhedron[M, V, Rational], out: Writer): Unit = {
    if (poly.nVertices > 0)
      writeVertices(poly.mV, out)
    if (poly.nRays > 0)
      writeRays(poly.mR, out)
  }

  def write(data: VData[M, V], out: Writer): Unit = {
    writeDim(data.polyhedron.nX, out)
    data.names.foreach { seq => writeNames(seq, out) }
    writePolyhedron(data.polyhedron, out)
    if (data.maps.nonEmpty) writeMaps(data.maps, data.names.get, out)
  }
}
