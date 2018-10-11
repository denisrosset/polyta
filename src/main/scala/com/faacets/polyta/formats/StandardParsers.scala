package com.faacets
package polyta
package formats

import java.math.BigInteger

import spire.math.{Rational, SafeLong}

import fastparse.noApi._
import White._

object StandardParsers {

  val lineEnding: P[Unit] = P("\r".? ~ "\n")

  val lineEndings: P[Unit] = lineEnding.rep(min=1)

  val nonNegativeInt: P[Int] = P( CharIn('0'to'9').repX(1).!.map(_.toInt) )

  val positiveInt: P[Int] = nonNegativeInt.filter(_ > 0)

  val negativeInt: P[Int] = P( "-" ~ nonNegativeInt ).map(i => -i)

  val int: P[Int] = negativeInt | nonNegativeInt

  val nonNegativeSafeLong: P[SafeLong] = P( CharIn('0'to'9').repX(1).!.map { s =>
    try {
      SafeLong(java.lang.Long.parseLong(s))
    } catch {
      case _: Exception => SafeLong(new BigInteger(s))
    }
  })

  val positiveSafeLong: P[SafeLong] = nonNegativeSafeLong.filter(_ > 0)

  val negativeSafeLong: P[SafeLong] = P( "-" ~ nonNegativeSafeLong ).map(i => -i)

  val safeLong: P[SafeLong] = P("-".!.? ~ nonNegativeSafeLong).map {
    case (Some(_), sl) => -sl
    case (None, sl) => sl
  }

  val rational: P[Rational] = P( safeLong ~ ("/" ~ positiveSafeLong).?  ).map {
    case (n, Some(d)) => Rational(n ,d)
    case (n, None) => Rational(n)
  }

  val nonNegativeRational: P[Rational] = P( nonNegativeSafeLong ~ ("/" ~ positiveSafeLong).? ).map {
    case (n, Some(d)) => Rational(n, d)
    case (n, None) => Rational(n)
  }

  val sign: P[Int] = P("+").map(x => 1) | P("-").map(x => -1)

  val signOptional: P[Int] = sign.?.map( _.getOrElse(1) )

  val rationalCoefficientSignOptional: P[Rational] = P( signOptional ~ nonNegativeRational.? ).map {
    case (s, optR) => s * optR.getOrElse(Rational.one)
  }

  val signedRationalCoefficient: P[Rational] = P( sign ~ nonNegativeRational.? ).map {
    case (s, optR) => s * optR.getOrElse(Rational.one)
  }

}
