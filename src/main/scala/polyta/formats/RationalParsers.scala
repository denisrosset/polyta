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

trait RationalParsers extends RegexParsers {

  val integerRegex = """(-|\+)?\d+""".r

  def sign = ("+" ^^^ 1) | ("-" ^^^ -1)

  def bigInt: Parser[BigInt] = integerRegex ^^ { BigInt(_) }

  def int: Parser[Int] = integerRegex ^^ { _.toInt }

  def nonNegativeBigInt: Parser[BigInt] = """\d+""".r ^^ { BigInt(_) }

  def nonNegativeInt: Parser[Int] = """\d+""".r ^^ { _.toInt }

  def positiveBigInt: Parser[BigInt] = """[1-9]\d*""".r ^^ { BigInt(_) }

  def positiveInt: Parser[Int] = """[1-9]\d*""".r ^^ { _.toInt }

  def nonNegativeRational: Parser[Rational] = nonNegativeBigInt ~ opt("/" ~> nonNegativeBigInt) ^^ {
    case n ~ Some(d) => Rational(n, d)
    case n ~ None => Rational(n, 1)
  }

  def rationalCoefficientSignOptional: Parser[Rational] = opt(sign) ~ opt(nonNegativeRational) ^^ {
    case optS ~ optR => optS.getOrElse(1) * optR.getOrElse(Field[Rational].one)
  }

  def signedRationalCoefficient: Parser[Rational] = sign ~ opt(nonNegativeRational) ^^ {
    case sign ~ optR => sign * optR.getOrElse(Field[Rational].one)
  }

  def rational: Parser[Rational] = bigInt ~ opt("/" ~> nonNegativeBigInt) ^^ {
    case n ~ Some(d) => Rational(n, d)
    case n ~ None => Rational(n, 1)
  }

}
