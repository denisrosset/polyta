package com.faacets
package polyta
package solvers
package porta

import java.io.{File, PrintWriter, BufferedReader, InputStreamReader, FileReader}
import org.scalatest.FunSuite
import org.scalacheck._

import spire.math.Rational

import qalg.algebra._
import qalg.algos._
import qalg.math._
import qalg.syntax.all._

import formats._
import formats.porta._

import Porta.DefaultOptions

class PortaConversion extends FunSuite {

  val path = "/com/faacets/polyta/solvers/porta/"
  val resources = ResourceListing.list(getClass, "." + path)
  val poiFiles = resources.filter(_.endsWith(".poi"))
  val ieqFiles = resources.filter(_.endsWith(".ieq"))

  def getReader(filename: String): java.io.Reader = {
    val url = getClass.getResource(path + filename)
    new BufferedReader(new InputStreamReader(url.openStream))
  }

  type V = DenseV[Rational]
  type M = DenseM[Rational]
  val V = VecInField[V, Rational]
  val M = MatVecInField[M, V, Rational]


  def compareMatricesArbColOrder(m1: M, m2: M): Unit = {
    val n = m1.nCols
    assert(n == m2.nCols)
    val cols1 = (0 until n).map(c => m1(::, c).toIndexedSeq).toSet
    val cols2 = (0 until n).map(c => m2(::, c).toIndexedSeq).toSet
    assert(cols1 == cols2)
  }

  def compareMatricesArbColOrderAndFactor(m1: M, m2: M): Unit = {
    val n = m1.nCols
    assert(n == m2.nCols)
    val cols1 = (0 until n).map(c => withPrimes(m1(::, c))._1.toIndexedSeq).toSet
    val cols2 = (0 until n).map(c => withPrimes(m2(::, c))._1.toIndexedSeq).toSet
    assert(cols1 == cols2)
  }


  def testPOI(filename: String): Unit = {
    val formatRead = POIData.FormatRead[DenseM[Rational], DenseV[Rational]]
    val reader = getReader(filename)
    val poi = formatRead.parse(reader).get
    val vpoly1 = poi.polyhedron
    val valid = if (vpoly1.vertices.nonEmpty) vpoly1.mV(::, 0) else V.fill(vpoly1.nX)(Rational.zero)
    val hpoly2 = Porta.toHPolyhedron(vpoly1)
    val vpoly3 = Porta.toVPolyhedron(hpoly2, valid)
    compareMatricesArbColOrder(vpoly1.mV, vpoly3.mV)
    compareMatricesArbColOrderAndFactor(vpoly1.mR, vpoly3.mR)
  }

  test("All .poi files can be converted to .ieq and back without change") {
    poiFiles.foreach { testPOI(_) }
  }
}
