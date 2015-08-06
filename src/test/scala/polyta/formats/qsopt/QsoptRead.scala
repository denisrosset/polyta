package com.faacets
package polyta
package formats
package qsopt

import java.io.{Reader, BufferedReader, InputStreamReader}
import org.scalatest.FunSuite
import org.scalacheck._

import spire.math.Rational

import qalg.algebra._
import qalg.math._

class QsoptRead extends FunSuite {
  import Matrix.packs._
  implicit val pack = PackFI[Rational]
  import pack._

  val path = "/com/faacets/polyta/formats/qsopt/"
  val resources = ResourceListing.list(getClass, "." + path)
  val solFilenames = resources.filter(_.endsWith(".sol"))

  def getReader(filename: String): Reader = {
    val url = getClass.getResource(filename)
    new BufferedReader(new InputStreamReader(url.openStream))
  }

  test("All .sol files can be parsed") {
    val formatRead = FormatRead[SolData]
    solFilenames.foreach { filename =>
      val res = formatRead.parse(getReader(filename))
      res.get
    }
  }
}
