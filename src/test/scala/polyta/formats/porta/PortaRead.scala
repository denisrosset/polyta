package com.faacets
package polyta
package formats
package porta

import java.io.{BufferedReader, InputStreamReader}
import org.scalatest.FunSuite
import org.scalacheck._

import spire.math.Rational

import qalg.algebra._
import qalg.math._

class PortaRead extends FunSuite {
  val path = "/com/faacets/polyta/formats/porta/"
  val resources = ResourceListing.list(getClass, "." + path)
  val poiFiles = resources.filter(_.endsWith(".poi"))
  val ieqFiles = resources.filter(_.endsWith(".ieq"))

  def getReader(filename: String): java.io.Reader = {
    val url = getClass.getResource(path + filename)
    new BufferedReader(new InputStreamReader(url.openStream))
  }

  test("All .poi files can be parsed") {
    val formatRead = POIData.FormatRead[DenseM[Rational], DenseV[Rational]]
    poiFiles.foreach { filename =>
      val reader = getReader(filename)
      formatRead.parse(reader).get
    }
  }

  test("All .ieq files can be parsed") {
    val formatRead = IEQData.FormatRead[DenseV[Rational]]
    ieqFiles.foreach { filename =>
      val reader = getReader(filename)
      formatRead.parse(reader).get
    }
  }
}
