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

import net.alasc.syntax.all._

import scalin.{Mat, Vec}
import scalin.immutable.dense._

class VDataWrite extends FormatWrite[VData] with PandaDataWrite {

  def writeMatrix(m: Mat[Rational], out: Writer): Unit = {
    cforRange(0 until m.nRows) { r =>
      Format.writeVectorSep[Rational](m(r, ::), " ", out)
      out.write("\n")
    }
  }

  def writeVertices(vertices: Mat[Rational], out: Writer): Unit = {
    out.write("Vertices:\n")
    writeMatrix(vertices.t, out)
  }

  def writeRays(rays: Mat[Rational], out: Writer): Unit = {
    out.write("Rays:\n")
    writeMatrix(rays.t, out)
  }

  def writePolytope(poly: VPolytopeM[Rational], out: Writer): Unit = {
    if (poly.vertices.nonEmpty)
      writeVertices(poly.mV, out)
    if (poly.rays.nonEmpty)
      writeRays(poly.mR, out)
  }

  def write(data: VData, out: Writer): Unit = {
    writeDim(data.polytope.dim, out)
    data.names.foreach { seq => writeNames(seq, out) }
    writePolytope(data.polytope, out)
    if (data.maps.nonEmpty) writeMaps(data.maps, data.names.get, out)
  }

}
