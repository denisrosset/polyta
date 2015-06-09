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

class HDataWrite[M, V](implicit val alg: AlgMVF[M, V, Rational]) extends FormatWrite[HData[M, V]] with PandaDataWrite {

  def writeMatrix(m: M, v: V, namesOption: Option[Seq[String]], op: String, out: Writer): Unit = {
    cforRange(0 until m.nRows) { r =>
      namesOption match {
        case Some(names) =>
          Format.writeVector[V, Rational](m(r, ::), names, out)
          out.write(" ")
          out.write(op)
          out.write(" ")
          out.write(v(r).toString)
        case None =>
          Format.writeVectorSep[V, Rational](m(r, ::), " ", out)
          out.write(" ")
          out.write((-v(r)).toString)
      }
      out.write("\n")
    }
  }

  def writeEquations(mAeq: M, vbeq: V, namesOption: Option[Seq[String]], out: Writer): Unit = {
    out.write("Equations:\n")
    writeMatrix(mAeq, vbeq, namesOption, "=", out)
  }

  def writeInequalities(mA: M, vb: V, namesOption: Option[Seq[String]], out: Writer): Unit = {
    out.write("Inequalities:\n")
    writeMatrix(mA, vb, namesOption, "<=", out)
  }

  def writePolyhedron(poly: HPolyhedronM[M, V, Rational], namesOption: Option[Seq[String]], out: Writer): Unit = {
    if (poly.equalities.nonEmpty)
      writeEquations(poly.mAeq, poly.vbeq, namesOption, out)
    if (poly.facets.nonEmpty)
      writeInequalities(poly.mA, poly.vb, namesOption, out)
  }

  def write(data: HData[M, V], out: Writer): Unit = {
    writeDim(data.polyhedron.nX, out)
    data.names.foreach { seq => writeNames(seq, out) }
    writePolyhedron(data.polyhedron, data.names, out)
    if (data.maps.nonEmpty) writeMaps(data.maps, data.names.get, out)
  }
}
