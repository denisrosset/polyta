package com.faacets
package polyta
package formats
package sympol

import java.io.Writer

import spire.math.Rational
import spire.syntax.cfor._

import scalin.immutable.dense._

class IneDataWrite extends FormatWrite[IneData] with SympolDataWrite {

  def writeHeader(upToSymmetry: Boolean, out: Writer): Unit = {
    out.write("H-representation\n")
    if (upToSymmetry) out.write("* UP TO SYMMETRY\n")
  }

  def writePolytope(poly: HPolytopeM[Rational], equalityRows: Set[Int], out: Writer): Unit = {
    val n = poly.equalities.size + poly.allFacets.size
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
    out.write((poly.dim + 1).toString)
    out.write(" rational\n")
    var ineqR = 0
    var eqR = 0
    cforRange(0 until n) { r =>
      if (equalityRows.contains(r)) {
        out.write(poly.equalities(eqR).rhs.toString)
        out.write(" ")
        Format.writeVectorSep[Rational](-poly.equalities(eqR).lhs, " ", out)
        eqR += 1
      } else {
        val ineqGE = poly.allFacets(ineqR).inequality.toGE
        out.write((-ineqGE.rhs).toString)
        out.write(" ")
        Format.writeVectorSep[Rational](ineqGE.lhs, " ", out)
        ineqR += 1
      }
      out.write("\n")
    }
    out.write("end\n")
  }

  def write(data: IneData, out: Writer): Unit = {
    writeHeader(data.symmetryInfo.fold(false)(_.upToSymmetryWRTO), out)
    writePolytope(data.polytope, data.equalityRows, out)
    data.symmetryInfo.foreach { writeSymmetryInfo(_, out) }
  }

}
