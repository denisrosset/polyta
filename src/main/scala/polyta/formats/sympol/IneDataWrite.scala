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

class IneDataWrite[M, V](implicit val M: MatVecInField[M, V, Rational]) extends FormatWrite[IneData[M, V]] with SympolDataWrite[M, V] {

  def writeHeader(out: Writer): Unit = {
    out.write("H-representation\n")
    out.write("begin\n")
  }

  def writeDimension(d: Int, nIneqs: Int, out: Writer): Unit = {
    out.write(nIneqs.toString)
    out.write(" ")
    out.write((d + 1).toString)
    out.write(" rational\n")
  }

  def writeIneqs(mA: M, vb: V, out: Writer): Unit = {
    cforRange(0 until mA.nRows) { r =>
      out.write(vb(r).toString)
      out.write(" ")
      Format.writeVectorSep[V, Rational](-mA(r, ::), " ", out)
      out.write("\n")
    }
  }

  def writePolyhedron(poly: HFullPolyhedron[M, V, Rational], out: Writer): Unit = {
    writeDimension(poly.nX, poly.nIneqs, out)
    writeIneqs(poly.mA, poly.vb, out)
    out.write("end\n")
  }

  def write(data: IneData[M, V], out: Writer): Unit = {
    writeHeader(out)
    writePolyhedron(data.polyhedron, out)
    data.symmetryInfo.foreach( writeSymmetryInfo(_, out) )
  }
}
