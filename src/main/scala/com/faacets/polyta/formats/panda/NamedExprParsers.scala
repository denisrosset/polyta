package com.faacets
package polyta
package formats
package panda

import spire.math.Rational
import spire.std.map._
import spire.syntax.field._

import fastparse.noApi._

trait NamedExprParsers extends PandaDataParsers with RationalParsers {

  val onlyVariable: P[(Rational, String)] = variable.map( str => (Rational.one, str) )

  val onlyCoefficient: P[(Rational, String)] = nonNegativeRational.map( rat => (rat, "") )

  val coefficientAndVariable: P[(Rational, String)] = (nonNegativeRational ~ variable)

  val positiveTerm: P[(Rational, String)] = P( coefficientAndVariable | onlyVariable | onlyCoefficient )

  val firstTerm: P[(Rational, String)] = P( sign.? ~ positiveTerm ).map {
    case (Some(-1), (rat, str)) => (-rat, str)
    case (other, term) => term
  }

  val nextTerm: P[(Rational, String)] = P( sign ~ positiveTerm ).map {
    case (-1, (rat, str)) => (-rat, str)
    case (other, term) => term
  }

  val expr: P[Map[String, Rational]] = P( firstTerm ~ nextTerm.rep ).map {
    case (rat, str, next) => (Map(str -> rat) /: next) {
      case (map, (r, v)) => Map(v -> r) + map
    }
  }

}
