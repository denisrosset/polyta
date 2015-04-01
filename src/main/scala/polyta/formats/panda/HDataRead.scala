package com.faacets
package polyta
package formats
package panda

import scala.util.parsing.combinator._

import spire.math.Rational

import qalg.algebra._

class HDataRead[V](implicit val V: VecInField[V, Rational]) extends FormatRead[HData] {

  object Parser extends ParserBase with PandaDataParser[V] with OptionParser  {
    implicit def V: VecInField[V, Rational] = HDataRead.this.V
    def hNamesSection: Parser[HData] = namesSection.map(seq => HData(names = Some(seq)))
    def hMapsSection: Parser[HData] = mapsSection.map(seq => HData(maps = seq))
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
      case first ~ next => Map(first._1 -> first._2) ++ next
    }

    def operator: Parser[ComparisonOperator] = ("<=" ^^^ LE) | ("=" ^^^ EQ) | (">=" ^^^ GE)

    def namedConstraint: Parser[NamedConstraint[Rational]] = expr ~ operator ~ expr ^^ {
      case lhs ~ op ~ rhs => NamedConstraint(None, lhs, op, rhs)
    }

    def vecConstraint: Parser[VecConstraint[V, Rational]] = rep1(rational) ^^ { seq =>
      VecConstraint(V.build(seq.init:_*), LE, -seq.last)
    }

    def constraint: Parser[Constraint[Rational]] = namedConstraint | vecConstraint

    def constraintsHeading = "Equations" | "Inequalities" | "INEQUALITY_SECTION"
    def constraintsSection: Parser[HData] = ((constraintsHeading ~ lineEndings) ~> rep(constraint <~ lineEndings)) ^^ {
      seq => HData(constraints = seq)
    }

    def hDimSection: Parser[HData] = dimSection.map(d => HData(dim = Some(d)))
    def section: Parser[HData] = hDimSection | hNamesSection | constraintsSection | hMapsSection
    def sections: Parser[HData] = rep1(section) into { secs =>
      (success(secs.head) /: secs.tail) {
        case (result, section) => result.flatMap { prevSection =>
          for {
            nextDim <- oneOptionOutOf(prevSection.dim, section.dim)
            nextNames <- oneOptionOutOf(prevSection.names, section.names)
            nextConstraints = prevSection.constraints ++ section.constraints
            nextMaps = prevSection.maps ++ section.maps
          } yield HData(nextDim, nextNames, nextConstraints, nextMaps)
        }
      }
    }

    def data = phrase(sections)
  }
}
