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

final class IEQDataWrite[M, V](implicit val alg: AlgMVF[M, V, Rational]) extends FormatWrite[IEQData[M, V]] {

  def writeDim(d: Int, out: Writer): Unit = {
    out.write("DIM = ")
    out.write(d.toString)
    out.write("\n\n")
  }

  def writeValid(valid: V, out: Writer): Unit = {
    out.write("VALID\n")
    Format.writeVectorSep[V, Rational](valid, " ", out)
    out.write("\n")
  }

  def writeLowerBounds(lowerBounds: V, out: Writer): Unit = {
    out.write("LOWER_BOUNDS\n")
    Format.writeVectorSep[V, Rational](lowerBounds, " ", out)
    out.write("\n")
  }

  def writeUpperBounds(upperBounds: V, out: Writer): Unit = {
    out.write("UPPER_BOUNDS\n")
    Format.writeVectorSep[V, Rational](upperBounds, " ", out)
    out.write("\n")
  }

  def writeEliminationOrder(d: Int, eliminationOrder: Seq[Int], out: Writer): Unit = {
    val indices = (eliminationOrder zip (1 to eliminationOrder.size)).toMap
    var prefix = ""
    out.write("ELIMINATION_ORDER\n")
    cforRange(0 until d) { k =>
      out.write(prefix)
      out.write(indices.getOrElse(k, 0).toString)
      prefix = " "
    }
    out.write("\n")
  }

  def writePolyhedron(dim: Int, poly: HPolyhedronM[M, V, Rational], out: Writer): Unit = {
    out.write("INEQUALITIES_SECTION\n")
    val names = Format.x1toN(dim)

    poly.facets.indices.foreach { r =>
        Format.writeVector[V, Rational](poly.mA(r, ::), names, out)
      out.write(" <= ")
      out.write(poly.vb(r).toString)
      out.write("\n")
    }

    poly.equalities.indices.foreach { r =>
      Format.writeVector[V, Rational](poly.mAeq(r, ::), names, out)
      out.write(" == ")
      out.write(poly.vbeq(r).toString)
      out.write("\n")
    }

    out.write("\n")
  }

  def writeEnd(out: Writer): Unit =
    out.write("END\n")

  def write(data: IEQData[M, V], out: Writer): Unit = {
    val dim = data.polyhedron.nX
    writeDim(dim, out)
    data.validPoint.foreach { writeValid(_, out) }
    data.lowerBounds.foreach { writeLowerBounds(_, out) }
    data.upperBounds.foreach { writeUpperBounds(_, out) }
    data.eliminationOrder.foreach { writeEliminationOrder(dim, _, out) }
    writePolyhedron(dim, data.polyhedron, out)
    writeEnd(out)
  }
}
