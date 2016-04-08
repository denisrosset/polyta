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
import scalin.immutable.dense._
import scalin.immutable.{DenseMat => IMat, DenseVec => IVec}

trait PortaDataParsers extends RationalParsers with AgnosticLineEndingParsers {

  override val whiteSpace = """([ \t])+""".r

  def dimSection: Parser[Int] = ("DIM" ~ "=") ~> positiveInt

  def rowVector(d: Int): Parser[Vec[Rational]] = repN(d, rational) ^^ { seq => IVec.tabulate(seq.size)(seq(_)) }

  def lineNumber = "(" ~ nonNegativeInt ~ ")"

  def end = "END" ~ opt(lineEndings)
}
