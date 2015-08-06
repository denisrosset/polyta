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
  import Matrix.packs._
  implicit val pack = PackFI[Rational]
  import pack._

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
    val formatRead = IneData.FormatRead[V]
    ineFilenames.foreach { filename =>
      val res = formatRead.parse(getReader(filename))
      res.get
    }
  }

  test("All .ext files can be parsed") {
    val formatRead = ExtData.FormatRead[V]
    extFilenames.foreach { filename =>
      val res = formatRead.parse(getReader(filename))
      res.get
    }    
  }

  test("All .auto files can be parsed") {
    val formatRead = SymmetryInfo.FormatRead
    autoFilenames.foreach { filename =>
      val res = formatRead.parse(getReader(filename))
      res.get
    }
  }
}
