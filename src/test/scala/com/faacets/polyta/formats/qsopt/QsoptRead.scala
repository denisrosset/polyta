package com.faacets
package polyta
package formats
package qsopt

import java.io.{Reader, BufferedReader, InputStreamReader}
import org.scalatest.FunSuite
import org.scalacheck._

import spire.math.Rational

class QsoptRead extends FunSuite {

  val path = "/com/faacets/polyta/formats/qsopt/"
  val resources = ResourceListing.list(getClass, "." + path)
  val solFilenames = resources.filter(_.endsWith(".sol"))

  def getContent(filename: String): String = {
    val url = getClass.getResource(filename)
    scala.io.Source.fromInputStream(url.openStream, "UTF-8").mkString
  }

  test("All .sol files can be parsed") {
    val formatRead = SolData.formatRead
    solFilenames.foreach { filename =>
      val res = formatRead.data.parse(getContent(filename))
      res.get
    }
  }

}
