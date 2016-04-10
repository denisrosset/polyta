package com.faacets
package polyta
package formats
package porta

import spire.math.Rational

import scalin.Vec
import scalin.immutable.{DenseVec => IVec}

trait PortaDataParsers extends RationalParsers with AgnosticLineEndingParsers {

  override val whiteSpace = """([ \t])+""".r

  def dimSection: Parser[Int] = ("DIM" ~ "=") ~> positiveInt

  def rowVector(d: Int): Parser[Vec[Rational]] = repN(d, rational) ^^ { seq => IVec.tabulate(seq.size)(seq(_)) }

  def lineNumber = "(" ~ nonNegativeInt ~ ")"

  def end = "END" ~ opt(lineEndings)
}
