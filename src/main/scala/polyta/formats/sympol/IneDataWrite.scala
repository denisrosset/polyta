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

class IneDataWrite[V](implicit val V: VecInField[V, Rational]) extends FormatWrite[IneData[V]] with SympolDataWrite {

  def writeHeader(upToSymmetry: Boolean, out: Writer): Unit = {
    out.write("H-representation\n")
    if (upToSymmetry) out.write("* UP TO SYMMETRY\n")
  }

  def writePolyhedron(poly: HPolyhedron[V, Rational], equalityRows: Set[Int], out: Writer): Unit = {
    val n = poly.equalities.size + poly.inequalities.size
    require(equalityRows.size == poly.equalities.size)
    if (equalityRows.nonEmpty) {
      out.write("linearity ")
      out.write(equalityRows.size)
      out.write(" ")
      out.write(equalityRows.toSeq.sorted.map(r => (r + 1).toString).mkString(" "))
      out.write("\n")
    }
    out.write("begin\n")
    out.write(n.toString)
    out.write(" ")
    out.write((poly.nX + 1).toString)
    out.write(" rational\n")
    var ineqR = 0
    var eqR = 0
    cforRange(0 until n) { r =>
      if (equalityRows.contains(r)) {
        out.write(poly.equalities(eqR).rhs.toString)
        out.write(" ")
        Format.writeVectorSep[V, Rational](-poly.equalities(eqR).lhs, " ", out)
        eqR += 1
      } else {
        val ineqGE = poly.inequalities(ineqR).toGE
        out.write((-ineqGE.rhs).toString)
        out.write(" ")
        Format.writeVectorSep[V, Rational](ineqGE.lhs, " ", out)
        ineqR += 1
      }
      out.write("\n")
    }
    out.write("end\n")
  }

  def write(data: IneData[V], out: Writer): Unit = {
    writeHeader(data.symmetryInfo.fold(false)(_.upToSymmetryWRTO), out)
    writePolyhedron(data.polyhedron, data.equalityRows, out)
    data.symmetryInfo.foreach { writeSymmetryInfo(_, out) }
  }
}
