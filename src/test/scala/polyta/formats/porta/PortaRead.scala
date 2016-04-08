package com.faacets
package polyta
package formats
package porta

import java.io.{BufferedReader, InputStreamReader}
import org.scalatest.FunSuite
import org.scalacheck._

import spire.math.Rational

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
    import POIData.FormatRead
    poiFiles.foreach { filename =>
      val reader = getReader(filename)
      FormatRead.parse(reader).get
    }
  }

  test("All .ieq files can be parsed") {
    import IEQData.FormatRead
    ieqFiles.foreach { filename =>
      val reader = getReader(filename)
      FormatRead.parse(reader).get
    }
  }
}
