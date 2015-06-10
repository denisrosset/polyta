package com.faacets
package polyta
package formats
package sympol

import java.io.{Reader, Writer}

import scala.{specialized => sp}

import scala.collection.{BitSet, SortedSet}

import spire.algebra._
import spire.algebra.partial._
import spire.math.Rational
import spire.syntax.group._
import spire.syntax.order._
import spire.syntax.action._
import spire.syntax.vectorSpace._
import spire.syntax.cfor._
import spire.util._

import qalg.algebra._
import qalg.algos._
import qalg.math._
import qalg.syntax.all._

import net.alasc.math._
import net.alasc.std.seq._
import net.alasc.syntax.all._

trait SympolDataWrite extends Any {

  def writePermutation1(perm: Perm, out: Writer): Unit = {
    val cycles = (perm + 1).to[Cycles]
    val elements = cycles.seq.map(_.seq.mkString(" "))
    out.write(elements.mkString(","))
  }

  def writeSymmetryInfo(si: SymmetryInfo, out: Writer): Unit = {
    out.write("permutation group\n")
    si.order.foreach { o =>
      out.write("* order ")
      out.write(o.toString)
      out.write("\n")
    }
    if (si.upToSymmetryWRTO)
      out.write("* w.r.t. to the original inequalities/vertices\n")
    out.write(si.generators.size.toString)
    out.write("\n")
    si.generators.foreach { perm =>
      out.write("  ")
      writePermutation1(perm, out)
      out.write("\n")
    }
    out.write(si.base.size.toString)
    out.write("\n")
    out.write("  ")
    out.write(si.base.mkString(" "))
    out.write("\n")
  }
}
