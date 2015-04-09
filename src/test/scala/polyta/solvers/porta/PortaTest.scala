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


  def compareMatricesArbRowOrder(m1: M, m2: M): Unit = {
    val n = m1.nRows
    assert(n == m2.nRows)
    val rows1 = (0 until n).map(r => m1(r, ::).toIndexedSeq).toSet
    val rows2 = (0 until n).map(r => m2(r, ::).toIndexedSeq).toSet
    assert(rows1 == rows2)
  }

  def compareMatricesArbRowOrderAndFactor(m1: M, m2: M): Unit = {
    val n = m1.nRows
    assert(n == m2.nRows)
    val rows1 = (0 until n).map(r => withPrimes(m1(r, ::))._1.toIndexedSeq).toSet
    val rows2 = (0 until n).map(r => withPrimes(m2(r, ::))._1.toIndexedSeq).toSet
    assert(rows1 == rows2)
  }


  def testPOI(filename: String): Unit = {
    val formatRead = POIData.FormatRead[DenseM[Rational], DenseV[Rational]]
    val reader = getReader(filename)
    val poi = formatRead.parse(reader).get
    val vpoly1 = poi.polyhedron
    val valid = if (vpoly1.nVertices > 0) vpoly1.mV(0, ::) else vpoly1.mR(0, ::)
    val hpoly2 = Porta.toHPolyhedron(vpoly1)
    val vpoly3 = Porta.toVPolyhedron(hpoly2, valid)
    compareMatricesArbRowOrder(vpoly1.mV, vpoly3.mV)
    compareMatricesArbRowOrderAndFactor(vpoly1.mR, vpoly3.mR)
  }

  test("All .poi files can be converted to .ieq and back without change") {
    poiFiles.foreach { testPOI(_) }
  }
}
