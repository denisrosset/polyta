package com.faacets
package polyta
package solvers

import java.io.{File, PrintWriter, BufferedReader, InputStreamReader, FileReader}
import org.scalatest.FunSuite
import org.scalacheck._

import spire.math.Rational

import scalin.{Vec, Mat}
import scalin.immutable.dense._
import scalin.syntax.all._

import formats._
import formats.porta._

object PortaTest {

  implicit class RichVec[A](val vec: Vec[A]) extends AnyVal {

    def withPrimes(implicit A: LinAlg[A]): Vec[A] = {
      import A.IVec
      vec / vec.gcd
    }

  }

}

class PortaTest extends FunSuite {

  import PortaTest._

  val path = "/com/faacets/polyta/solvers/porta/"
  val resources = ResourceListing.list(getClass, "." + path)
  val poiFiles = resources.filter(_.endsWith(".poi"))
  val ieqFiles = resources.filter(_.endsWith(".ieq"))


  def getContent(filename: String): String = {
    val url = getClass.getResource(path + filename)
    scala.io.Source.fromInputStream(url.openStream, "UTF-8").mkString
  }

  def compareMatricesArbRowOrder(m1: Mat[Rational], m2: Mat[Rational]): Unit = {
    val n = m1.nRows
    assert(n == m2.nRows)
    val rows1 = (0 until n).map(r => m1(r, ::).toIndexedSeq).toSet
    val rows2 = (0 until n).map(r => m2(r, ::).toIndexedSeq).toSet
    assert(rows1 == rows2)
  }

  def compareMatricesArbRowOrderAndFactor(m1: Mat[Rational], m2: Mat[Rational]): Unit = {
    val n = m1.nRows
    assert(n == m2.nRows)
    val rows1 = (0 until n).map(r => m1(r, ::).withPrimes.toIndexedSeq).toSet
    val rows2 = (0 until n).map(r => m2(r, ::).withPrimes.toIndexedSeq).toSet
    assert(rows1 == rows2)
  }


  def testPOI(filename: String): Unit = {
    val formatRead = POIData.formatRead
    val content = getContent(filename)
    val poi = formatRead.data.parse(content).get.value
    val vpoly1 = poi.polytope
    val valid = if (vpoly1.mV.nRows > 0) vpoly1.mV(0, ::) else zeros[Rational](vpoly1.dim)
    val hpoly2 = Porta.toHPolytope(vpoly1)
    val vpoly3 = Porta.toVPolytope(hpoly2, valid)
    compareMatricesArbRowOrder(vpoly1.mV, vpoly3.mV)
    compareMatricesArbRowOrderAndFactor(vpoly1.mR, vpoly3.mR)
  }

  test("All .poi files can be converted to .ieq and back without change") {
    poiFiles.foreach { testPOI(_) }
  }

}