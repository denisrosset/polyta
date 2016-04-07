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

import scalin.{Mat, Vec}

trait PortaDataParsers[V] extends RationalParsers with AgnosticLineEndingParsers {
  implicit def Q: LinAlg[Rational]

  override val whiteSpace = """([ \t])+""".r

  def dimSection: Parser[Int] = ("DIM" ~ "=") ~> positiveInt

  def rowVector(d: Int): Parser[Vec[Rational]] = repN(d, rational) ^^ { Q.IVec.fromSeq(_) }

  def lineNumber = "(" ~ nonNegativeInt ~ ")"

  def end = "END" ~ opt(lineEndings)
}
