package com.faacets
package polyta
package formats
package sympol

import java.io.Writer


import spire.math.Rational
import spire.syntax.cfor._

class ExtDataWrite extends FormatWrite[ExtData] with SympolDataWrite {

  def writeHeader(upToSymmetry: Boolean, out: Writer): Unit = {
    out.write("V-representation\n")
    if (upToSymmetry) out.write("* UP TO SYMMETRY\n")
  }

  def writePolytope(poly: VPolytopeM[Rational], rayRows: Set[Int], out: Writer): Unit = {
    out.write("begin\n")
    val n = poly.allVertices.size + poly.allRays.size
    require(rayRows.size == poly.allRays.size)
    out.write(n.toString)
    out.write(" ")
    out.write((poly.dim + 1).toString)
    out.write(" rational\n")
    var vertC = 0
    var rayC = 0
    cforRange(0 until n) { c =>
      if (rayRows.contains(c)) {
        out.write("0 ")
        Format.writeVectorSep[Rational](poly.allRays(rayC).point, " ", out)
        rayC += 1
      } else {
        out.write("1 ")
        Format.writeVectorSep[Rational](poly.allVertices(vertC).point, " ", out)
        vertC += 1
      }
      out.write("\n")
    }
    out.write("end\n")
  }

  def write(data: ExtData, out: Writer): Unit = {
    writeHeader(data.symmetryInfo.fold(false)(_.upToSymmetryWRTO), out)
    writePolytope(data.polytope, data.rayRows, out)
    data.symmetryInfo.foreach { writeSymmetryInfo(_, out) }
  }

}
