package com.faacets
package polyta
package formats

import scala.{specialized => sp}

import scala.util.parsing.combinator._

import spire.algebra._
import spire.math.Rational
import spire.syntax.order._
import spire.syntax.vectorSpace._
import spire.syntax.cfor._
import spire.util._

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

trait RationalParser extends RegexParsers {
  def sign = ("+" ^^^ 1) | ("-" ^^^ -1)
  def bigInt: Parser[BigInt] = """-?\d+""".r ^^ { BigInt(_) }
  def positiveBigInt: Parser[BigInt] = """\d+""".r ^^ { BigInt(_) }

  def positiveRational: Parser[Rational] = positiveBigInt ~ opt("/" ~> positiveBigInt) ^^ {
    case n ~ Some(d) => Rational(n, d)
    case n ~ None => Rational(n, 1)
  }
  def rationalCoefficientSignOptional: Parser[Rational] = opt(sign) ~ opt(positiveRational) ^^ {
    case optS ~ optR => optS.getOrElse(1) * optR.getOrElse(Field[Rational].one)
  }
  def signedRationalCoefficient: Parser[Rational] = sign ~ opt(positiveRational) ^^ {
    case sign ~ optR => sign * optR.getOrElse(Field[Rational].one)
  }
  def rational: Parser[Rational] = bigInt ~ opt("/" ~> positiveBigInt) ^^ {
    case n ~ Some(d) => Rational(n, d)
    case n ~ None => Rational(n, 1)
  }
}
