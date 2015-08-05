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

class ExtDataWrite[V](implicit val V: VecField[V, Rational]) extends FormatWrite[ExtData[V]] with SympolDataWrite {

  def writeHeader(upToSymmetry: Boolean, out: Writer): Unit = {
    out.write("V-representation\n")
    if (upToSymmetry) out.write("* UP TO SYMMETRY\n")
  }

  def writePolytope(poly: VPolytope[V, Rational], rayCols: Set[Int], out: Writer): Unit = {
    out.write("begin\n")
    val n = poly.vertices.size + poly.rays.size
    require(rayCols.size == poly.rays.size)
    out.write(n.toString)
    out.write(" ")
    out.write((poly.nX + 1).toString)
    out.write(" rational\n")
    var vertC = 0
    var rayC = 0
    cforRange(0 until n) { c =>
      if (rayCols.contains(c)) {
        out.write("0 ")
        Format.writeVectorSep[V, Rational](poly.rays(rayC).point, " ", out)
        rayC += 1
      } else {
        out.write("1 ")
        Format.writeVectorSep[V, Rational](poly.vertices(vertC).point, " ", out)
        vertC += 1
      }
      out.write("\n")
    }
    out.write("end\n")
  }

  def write(data: ExtData[V], out: Writer): Unit = {
    writeHeader(data.symmetryInfo.fold(false)(_.upToSymmetryWRTO), out)
    writePolytope(data.polytope, data.rayCols, out)
    data.symmetryInfo.foreach { writeSymmetryInfo(_, out) }
  }
}
