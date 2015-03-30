package com.faacets
package polyta
package formats

import java.io.Writer

import scala.{specialized => sp}

import scala.collection.BitSet

import spire.algebra._
import spire.math.Rational
import spire.syntax.order._
import spire.syntax.vectorSpace._
import spire.syntax.cfor._
import spire.util._

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

object Format {
  /** Writes an affine expression represented as a map of variable names to
    * the corresponding coefficients.
    * 
    * Special case: the empty key "" is a scalar coefficient, i.e.
    * Map("" -> 2, "x" -> 3) is equal to "3 x + 2".
    */
  def writeVector[@sp(Double) A: Field: Order](vec: Map[String, A], out: Writer): Unit = {
    var wroteSomething: Boolean = false
    var plusString = ""
    val minusString = "-"
    var space = ""
    val keysIt = vec.keysIterator
    val zero = Field[A].zero
    val minusOne = -Field[A].one
    while (keysIt.hasNext) {
      val k = keysIt.next
      val v = vec(k)
      val sign = v.compare(zero)
      if (sign > 0) {
        out.write(space)
        out.write(plusString)
        out.write(" ")
        if (k.nonEmpty || !v.isOne) {
          out.write(v.toString)
          out.write(" ")
        }
        out.write(k)
      } else if (sign < 0) {
        out.write(space)
        out.write(minusString)
        out.write(" ")
        if (k.nonEmpty || v =!= minusOne) {
          out.write((-v).toString)
          out.write(" ")
        }
        out.write(k)
      }
      if (sign != 0) {
        wroteSomething = true
        plusString = "+"
        space = " "
      }
    }
    if (!wroteSomething) out.write(zero.toString)
  }

  def writeVectorSep[V, @sp(Double) A: Field: Order](vec: V, sep: String, out: Writer)(implicit V: Vec[V, A]): Unit = {
    var prefix = ""
    cforRange(0 until vec.length) { k =>
      out.write(prefix)
      out.write(vec(k).toString)
      prefix = sep
    }
    out.write("\n")
  }

  def writeVector[V, @sp(Double) A: Field: Order](vec: V, variableNames: Seq[String], out: Writer)(implicit V: Vec[V, A]): Unit = {
    var wroteSomething: Boolean = false
    var plusString = ""
    val minusString = "-"
    var space = ""
    val zero = Field[A].zero
    val minusOne = -Field[A].one
    cforRange(0 until vec.length) { k =>
      val v = vec(k)
      val sign = v.compare(zero)
      if (sign > 0) {
        out.write(space)
        out.write(plusString)
        out.write(" ")
        if (!v.isOne) {
          out.write(v.toString)
          out.write(" ")
        }
        out.write(variableNames(k))
      } else if (sign < 0) {
        out.write(space)
        out.write(minusString)
        out.write(" ")
        if (v =!= minusOne) {
          out.write((-v).toString)
          out.write(" ")
        }
        out.write(variableNames(k))
      }
      if (sign != 0) {
        wroteSomething = true
        plusString = "+"
        space = " "
      }
    }
    if (!wroteSomething) out.write(zero.toString)
  }
}
