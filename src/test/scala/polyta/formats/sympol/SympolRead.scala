package com.faacets
package polyta
package formats
package sympol

import java.io.{Reader, BufferedReader, InputStreamReader}
import org.scalatest.FunSuite
import org.scalacheck._

import spire.math.Rational

import qalg.algebra._
import qalg.math._

class SympolRead extends FunSuite {
  val path = "/com/faacets/polyta/formats/sympol/"
  val resources = ResourceListing.list(getClass, "." + path)
  val extFilenames = resources.filter(_.endsWith(".ext"))
  val ineFilenames = resources.filter(_.endsWith(".ine"))

  def getReader(filename: String): Reader = {
    val url = getClass.getResource(filename)
    new BufferedReader(new InputStreamReader(url.openStream))
  }

  test("All .ine files can be parsed") {
    val formatRead = IneData.FormatRead[DenseM[Rational], DenseV[Rational]]
    ineFilenames.foreach { filename =>
      formatRead.parse(getReader(filename)).get
    }
  }

  test("All .ext files can be parsed") {
    val formatRead = ExtData.FormatRead[DenseM[Rational], DenseV[Rational]]
    extFilenames.foreach { filename =>
      formatRead.parse(getReader(filename)).get
    }
  }
}
