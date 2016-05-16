package com.faacets
package polyta
package formats
package porta

import org.scalatest.FunSuite

class PortaRead extends FunSuite {

  val path = "/com/faacets/polyta/formats/porta/"
  val resources = ResourceListing.list(getClass, "." + path)
  val poiFiles = resources.filter(_.endsWith(".poi"))
  val ieqFiles = resources.filter(_.endsWith(".ieq"))

  def getContent(filename: String): String = {
    val url = getClass.getResource(path + filename)
    scala.io.Source.fromInputStream(url.openStream, "UTF-8").mkString
  }

  test("All .poi files can be parsed") {
    import POIData.FormatRead
    poiFiles.foreach { filename =>
      val reader = getContent(filename)
      FormatRead.data.parse(reader).get
    }
  }

  test("All .ieq files can be parsed") {
    import IEQData.FormatRead
    ieqFiles.foreach { filename =>
      val reader = getContent(filename)
      FormatRead.data.parse(reader).get
    }
  }
}
