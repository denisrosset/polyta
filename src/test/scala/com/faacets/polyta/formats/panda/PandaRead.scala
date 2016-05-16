package com.faacets
package polyta
package formats
package panda

import java.io.{Reader, BufferedReader, InputStreamReader}
import org.scalatest.FunSuite
import org.scalacheck._

import spire.math.Rational

class PandaRead extends FunSuite {

  val paths = Seq("/com/faacets/polyta/formats/panda/panda_format/",
    "/com/faacets/polyta/formats/panda/porta_format/")

  val filenames = paths.flatMap { path =>
    ResourceListing.list(getClass, "." + path).map(path + _)
  }

  val vFilenames = filenames.filter(_.endsWith(".v"))
  val hFilenames = filenames.filter(_.endsWith(".h"))

  def getContent(filename: String): String = {
    val url = getClass.getResource(filename)
    scala.io.Source.fromInputStream(url.openStream, "UTF-8").mkString
  }

  test("All .h files can be parsed") {
    val formatRead = HData.formatRead
    hFilenames.foreach { filename =>
      println(filename)
      formatRead.data.parse(getContent(filename)).get
    }
  }

  test("All .v files can be parsed") {
    val formatRead = VData.formatRead
    vFilenames.foreach { filename =>
      formatRead.data.parse(getContent(filename)).get
    }
  }

}
