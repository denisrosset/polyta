package com.faacets
package polyta
package formats
package porta

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

trait IEQDataWrite[V] extends Any with FormatWrite[IEQData[V]] {
  implicit def V: VecInField[V, Rational]

  def writeDim(d: Int, out: Writer): Unit = {
    out.write("DIM = ")
    out.write(d.toString)
    out.write("\n\n")
  }

  def writeValid(valid: V, out: Writer): Unit = {
    out.write("VALID\n")
    Format.writeVectorSep[V, Rational](valid, " ", out)
  }

  def writeLowerBounds(lowerBounds: V, out: Writer): Unit = {
    out.write("LOWER_BOUNDS\n")
    Format.writeVectorSep[V, Rational](lowerBounds, " ", out)
  }

  def writeUpperBounds(upperBounds: V, out: Writer): Unit = {
    out.write("UPPER_BOUNDS\n")
    Format.writeVectorSep[V, Rational](upperBounds, " ", out)
  }

  def writeEliminationOrder(d: Int, eliminationOrder: Seq[Int], out: Writer): Unit = {
    val indices = (eliminationOrder zip (1 to eliminationOrder.size)).toMap
    var prefix = ""
    out.write("ELIMINATION_ORDER\n")
    cforRange(0 until d) { k =>
      out.write(prefix)
      out.write(indices.getOrElse(k, 0).toString)
      prefix = " "
    }
    out.write("\n")
  }

  def writeConstraints(dim: Int, constraints: Seq[VecConstraint[V, Rational]], out: Writer): Unit = {
    out.write("INEQUALITIES_SECTION\n")
    constraints.foreach { constraint =>
      Format.writeVector[V, Rational](constraint.lhs, Format.x1toN(dim), out)
      out.write(" ")
      out.write(constraint.op.toString)
      out.write(" ")
      out.write(constraint.rhs.toString)
      out.write("\n")
    }
    out.write("\n")
  }

  def writeEnd(out: Writer): Unit =
    out.write("END\n")

  def write(data: IEQData[V], out: Writer): Unit = {
    val dim = data.dim
    writeDim(dim, out)
    data.validPoint.foreach { writeValid(_, out) }
    data.lowerBounds.foreach { writeLowerBounds(_, out) }
    data.upperBounds.foreach { writeUpperBounds(_, out) }
    data.eliminationOrder.foreach { writeEliminationOrder(dim, _, out) }
    writeConstraints(dim, data.constraints, out)
    writeEnd(out)
  }
}
