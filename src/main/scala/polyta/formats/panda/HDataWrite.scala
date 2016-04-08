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

class HDataWrite extends FormatWrite[HData] with PandaDataWrite {

  def writeMatrix(m: Mat[Rational], v: Vec[Rational], namesOption: Option[Seq[String]], op: String, out: Writer): Unit = {
    cforRange(0 until m.nRows) { r =>
      namesOption match {
        case Some(names) =>
          Format.writeVector[Rational](m(r, ::), names, out)
          out.write(" ")
          out.write(op)
          out.write(" ")
          out.write(v(r).toString)
        case None =>
          Format.writeVectorSep[Rational](m(r, ::), " ", out)
          out.write(" ")
          out.write((-v(r)).toString)
      }
      out.write("\n")
    }
  }

  def writeEquations(mAeq: Mat[Rational], vbeq: Vec[Rational], namesOption: Option[Seq[String]], out: Writer): Unit = {
    out.write("Equations:\n")
    writeMatrix(mAeq, vbeq, namesOption, "=", out)
  }

  def writeInequalities(mA: Mat[Rational], vb: Vec[Rational], namesOption: Option[Seq[String]], out: Writer): Unit = {
    out.write("Inequalities:\n")
    writeMatrix(mA, vb, namesOption, "<=", out)
  }

  def writePolytope(poly: HPolytopeM[Rational], namesOption: Option[Seq[String]], out: Writer): Unit = {
    if (poly.equalities.nonEmpty)
      writeEquations(poly.mAeq, poly.vbeq, namesOption, out)
    if (poly.facets.nonEmpty)
      writeInequalities(poly.mA, poly.vb, namesOption, out)
  }

  def write(data: HData, out: Writer): Unit = {
    writeDim(data.polytope.dim, out)
    data.names.foreach { seq => writeNames(seq, out) }
    writePolyhedron(data.polytope, data.names, out)
    if (data.maps.nonEmpty) writeMaps(data.maps, data.names.get, out)
  }

}
