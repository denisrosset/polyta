package com.faacets
package polyta
package formats
package panda

import java.io.{Reader, BufferedReader, InputStreamReader}
import org.scalatest.FunSuite
import org.scalacheck._

import spire.math.Rational

import qalg.algebra._
import qalg.math._

class PandaRead extends FunSuite {
  val paths = Seq("/com/faacets/polyta/formats/panda/panda_format/",
    "/com/faacets/polyta/formats/panda/porta_format/")

  val filenames = paths.flatMap { path =>
    ResourceListing.list(getClass, "." + path).map(path + _)
  }

  val vFilenames = filenames.filter(_.endsWith(".v"))
  val hFilenames = filenames.filter(_.endsWith(".h"))

  def getReader(filename: String): Reader = {
    val url = getClass.getResource(filename)
    new BufferedReader(new InputStreamReader(url.openStream))
  }

  test("All .h files can be parsed") {
    val formatRead = HData.FormatRead[DenseM[Rational], DenseV[Rational]]
    hFilenames.foreach { filename =>
      formatRead.parse(getReader(filename)).get
    }
  }

  test("All .v files can be parsed") {
    val formatRead = VData.FormatRead[DenseM[Rational], DenseV[Rational]]
    vFilenames.foreach { filename =>
      formatRead.parse(getReader(filename)).get
    }
  }
}
