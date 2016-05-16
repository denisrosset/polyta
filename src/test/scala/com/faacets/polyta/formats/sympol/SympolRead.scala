package com.faacets
package polyta
package formats
package sympol

import org.scalatest.FunSuite

class SympolRead extends FunSuite {

  val path = "/com/faacets/polyta/formats/sympol/"
  val resources = ResourceListing.list(getClass, "." + path)
  val extFilenames = resources.filter(_.endsWith(".ext"))
  val ineFilenames = resources.filter(_.endsWith(".ine"))
  val autoFilenames = resources.filter(_.endsWith(".auto"))

  def getContent(filename: String): String = {
    val url = getClass.getResource(path + filename)
    scala.io.Source.fromInputStream(url.openStream, "UTF-8").mkString
  }

  test("All .ine files can be parsed") {
    import IneData.formatRead
    ineFilenames.foreach { filename =>
      val res = formatRead.data.parse(getContent(filename))
      res.get.value
    }
  }

  test("All .ext files can be parsed") {
    import ExtData.formatRead
    extFilenames.foreach { filename =>
      val res = formatRead.data.parse(getContent(filename))
      res.get.value
    }    
  }

  test("All .auto files can be parsed") {
    import SymmetryInfo.formatRead
    autoFilenames.foreach { filename =>
      val res = formatRead.data.parse(getContent(filename))
      res.get.value
    }
  }

}
