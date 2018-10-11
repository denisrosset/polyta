package com.faacets
package polyta
package formats

import java.io.Writer

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.order._
import spire.syntax.vectorSpace._
import spire.syntax.cfor._
import spire.util._

import scalin.Vec

object StandardWriters {
  /** Writes an affine expression represented as a map of variable names to
    * the corresponding coefficients.
    * 
    * Special case: the empty key "" is a scalar coefficient, i.e.
    * Map("" -> 2, "x" -> 3) is equal to "3 x + 2".
    */
  def writeVector[A: Field: Order](vec: Map[String, A], out: Writer): Unit = {
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

  def writeVectorSep[A: Field: Order](vec: Vec[A], sep: String, out: Writer): Unit = {
    var prefix = ""
    cforRange(0 until vec.length) { k =>
      out.write(prefix)
      out.write(vec(k).toString)
      prefix = sep
    }
  }

  def x1toN(n: Int): IndexedSeq[String] = new IndexedSeq[String] {
    def length = n
    def apply(k: Int) = "x" + (k + 1).toString
  }

  /** Writes a term in an expression.
    * 
    * @param coefficient Term coefficient
    * @param variableName Term variable name, or "" if the term is a constant
    * @param first Is this the first term in the sequence ?
    * @param out Writer
    * @param withSpaces Whether to write spaces between tokens
    * @param skipZero   Whether to skip the write of zero coefficients
    * 
    * @return false if anything is written, `first` otherwise
    */
  def writeTerm[A: Field: Order](coefficient: A, variableName: String, first: Boolean, out: Writer, withSpaces: Boolean = true, skipZero: Boolean = true): Boolean = {
    val sign = coefficient.compare(Field[A].zero)
    if (sign != 0 || !skipZero) {
      if (sign >= 0 && !first) {
        if (withSpaces) out.write(" ")
        out.write("+")
        if (withSpaces) out.write(" ")
      }
      if (sign < 0) {
        if (withSpaces) out.write(" ")
        out.write("-")
        if (withSpaces) out.write(" ")
      }
      val absCoefficient = if (sign >= 0) coefficient else -coefficient
      if (!absCoefficient.isOne || variableName.isEmpty) {
        out.write(absCoefficient.toString)
        if (withSpaces && variableName.nonEmpty) out.write(" ")
      }
      out.write(variableName)
      false
    } else first
  }

  def writeVector[@sp(Double) A: Field: Order](vec: Vec[A], variableNames: Seq[String], out: Writer, constantOpt: Opt[A] = Opt.empty[A], withSpaces: Boolean = true): Unit = {
    var first: Boolean = true
    cforRange(0 until vec.length) { k =>
      first = writeTerm(vec(k), variableNames(k), first, out, withSpaces = withSpaces, skipZero = true)
    }
    val constant = constantOpt.getOrElseFast(Field[A].zero)
    writeTerm(constant, "", first, out, withSpaces, skipZero = !first)
  }

}
