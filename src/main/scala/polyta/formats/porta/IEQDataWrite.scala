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

trait IEQDataWrite[M, V] extends Any {
  implicit def M: MatVecInField[M, V, Rational]
  implicit def V: VecInField[V, Rational] = M.V

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

  def writePolyhedron(polytope: HPolyhedron[M, V, Rational], out: Writer): Unit = {
    val d = polytope.nX
    val variableNames = new IndexedSeq[String] {
      def length = d
      def apply(k: Int) = "x" + (k + 1).toString
    }
    out.write("INEQUALITIES_SECTION\n")
    cforRange(0 until polytope.nEqs) { r =>
      Format.writeVector[FunV[Rational], Rational](polytope.mAeq.view(r, ::), variableNames, out)
      out.write(" == ")
      out.write(polytope.vbeq(r).toString)
      out.write("\n")
    }
    out.write("\n")
    cforRange(0 until polytope.nIneqs) { r =>
      Format.writeVector[FunV[Rational], Rational](polytope.mA.view(r, ::), variableNames, out)
      out.write(" == ")
      out.write(polytope.vb(r).toString)
      out.write("\n")
    }
    out.write("\n")
  }

  def writeEnd(out: Writer): Unit =
    out.write("END\n")

  def write(data: IEQData[M, V], out: Writer): Unit = {
    val dim = data.polyhedron.nX
    writeDim(dim, out)
    data.validPoint.foreach { writeValid(_, out) }
    data.lowerBounds.foreach { writeLowerBounds(_, out) }
    data.upperBounds.foreach { writeUpperBounds(_, out) }
    data.eliminationOrder.foreach { writeEliminationOrder(dim, _, out) }
    writePolyhedron(data.polyhedron, out)
    writeEnd(out)
  }
}
