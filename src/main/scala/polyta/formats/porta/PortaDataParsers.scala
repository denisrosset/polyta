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

trait PortaDataParsers[V] extends RationalParsers with AgnosticLineEndingParsers {
  implicit def alg: AlgVF[V, Rational]

  override val whiteSpace = """([ \t])+""".r

  def dimSection: Parser[Int] = ("DIM" ~ "=") ~> positiveInt

  def rowVector(d: Int): Parser[V] = repN(d, rational) ^^ { VecBuilder[V, Rational].build(_: _*) }

  def lineNumber = "(" ~ nonNegativeInt ~ ")"

  def end = "END" ~ opt(lineEndings)
}
