package com.faacets
package polyta
package formats
package panda

import java.io.{Reader, Writer}

import spire.syntax.action._

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

  def writeMaps(maps: Seq[Perm], names: Seq[String], out: Writer): Unit = {
    out.write("Maps:\n")
    val d = names.size
    maps.foreach { perm =>
      out.write((0 until d).map(k => names(k <|+| perm)).mkString(" "))
      out.write("\n")
    }
  }
}
