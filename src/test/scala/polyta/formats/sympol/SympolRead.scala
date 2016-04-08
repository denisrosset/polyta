package com.faacets
package polyta
package formats
package sympol

import java.io.{Reader, BufferedReader, InputStreamReader}
import org.scalatest.FunSuite
import org.scalacheck._

import spire.math.Rational

class SympolRead extends FunSuite {

  val path = "/com/faacets/polyta/formats/sympol/"
  val resources = ResourceListing.list(getClass, "." + path)
  val extFilenames = resources.filter(_.endsWith(".ext"))
  val ineFilenames = resources.filter(_.endsWith(".ine"))
  val autoFilenames = resources.filter(_.endsWith(".auto"))

  def getReader(filename: String): Reader = {
    val url = getClass.getResource(filename)
    new BufferedReader(new InputStreamReader(url.openStream))
  }

  test("All .ine files can be parsed") {
    import IneData.FormatRead
    ineFilenames.foreach { filename =>
      val res = FormatRead.parse(getReader(filename))
      res.get
    }
  }

  test("All .ext files can be parsed") {
    import ExtData.FormatRead
    extFilenames.foreach { filename =>
      val res = FormatRead.parse(getReader(filename))
      res.get
    }    
  }

  test("All .auto files can be parsed") {
    import SymmetryInfo.FormatRead
    autoFilenames.foreach { filename =>
      val res = FormatRead.parse(getReader(filename))
      res.get
    }
  }
}
