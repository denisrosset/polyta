package com.faacets
package polyta
package formats
package panda

import spire.math.Rational
import spire.std.map._
import spire.syntax.field._

trait NamedExprParsers extends PandaDataParsers with RationalParsers {

  def onlyVariable: Parser[(String, Rational)] = variable ^^ { str => (str, Rational.one) }

  def onlyCoefficient: Parser[(String, Rational)] = nonNegativeRational ^^ { rat => ("", rat) }

  def coefficientAndVariable: Parser[(String, Rational)] = nonNegativeRational ~ variable ^^ {
    case rat ~ str => (str, rat)
  }

  def positiveTerm: Parser[(String, Rational)] =
    coefficientAndVariable | onlyVariable | onlyCoefficient

  def firstTerm: Parser[(String, Rational)] = opt(sign) ~ positiveTerm ^^ {
    case ~(Some(-1), (str, rat)) => (str, -rat)
    case other ~ term => term
  }

  def nextTerm: Parser[(String, Rational)] = sign ~ positiveTerm ^^ {
    case ~(-1, (str, rat)) => (str, -rat)
    case other ~ term => term
  }

  def expr: Parser[Map[String, Rational]] = firstTerm ~ rep(nextTerm) ^^ {
    case first ~ next => (Map(first._1 -> first._2) /: next) {
      case (map, (v, r)) => Map(v -> r) + map
    }
  }

}
