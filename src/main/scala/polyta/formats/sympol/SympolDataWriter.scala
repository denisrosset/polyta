package com.faacets
package polyta
package formats
package sympol

import java.io.{Reader, Writer}

import scala.{specialized => sp}

import scala.collection.{BitSet, SortedSet}

import spire.algebra._
import spire.math.Rational
import spire.syntax.order._
import spire.syntax.vectorSpace._
import spire.syntax.cfor._
import spire.util._

import qalg.algebra._
import qalg.algos._
import qalg.math._
import qalg.syntax.all._

import net.alasc.math._
import net.alasc.syntax.all._

trait SympolDataWrite[M, V] extends Any {
  implicit def M: MatVecInField[M, V, Rational]
  implicit def V: VecInField[V, Rational] = M.V

  def writePermutation(perm: Perm, out: Writer): Unit = {
    val cycles = perm.to[Cycles]
    val elements = cycles.seq.map(_.seq.mkString(" "))
    out.write(elements.mkString(","))
  }

  def writeSymmetryInfo(si: SymmetryInfo, out: Writer): Unit = {
    out.write(si.generators.size)
    out.write("\n")
    si.generators.foreach { perm =>
      out.write("  ")
      writePermutation(perm, out)
      out.write("\n")
    }
    out.write(si.base.size)
    out.write("\n")
    out.write("  ")
    out.write(si.base.mkString(" "))
    out.write("\n")
  }
}
