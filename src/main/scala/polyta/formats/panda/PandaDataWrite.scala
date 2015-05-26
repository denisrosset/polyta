package com.faacets
package polyta
package formats
package panda

import java.io.{Reader, Writer}

import spire.math.Rational
import spire.syntax.action._

import qalg.algebra._

import net.alasc.math.Perm

trait PandaDataWrite extends Any {
  def writeDim(dim: Int, out: Writer): Unit = {
    out.write("DIM=")
    out.write(dim.toString)
    out.write("\n")
  }

  def writeNames(names: Seq[String], out: Writer): Unit = {
    out.write("Names:\n")
    out.write(names.mkString(" "))
    out.write("\n")
  }

  def writeMaps[M, V](maps: Seq[AffineTransform[M, V, Rational]], names: Seq[String], out: Writer)(implicit M: MatVecInField[M, V, Rational]): Unit = {
    val afw = new AffineTransformWrite[M, V](names)
    out.write("Maps:\n")
    maps.foreach { affineTransform =>
      afw.write(affineTransform, out)
      out.write("\n")
    }
  }
}
