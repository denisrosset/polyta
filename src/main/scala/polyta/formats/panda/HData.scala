package com.faacets
package polyta
package formats
package panda

import scala.util.parsing.combinator._

import spire.math.Rational

case class HData(
  names: Option[Seq[String]] = None,
  constraints: Seq[NamedConstraint[Rational]] = Seq.empty,
  maps: Seq[Seq[String]] = Seq.empty
)

trait HDataRead[V] extends FormatRead[HData] { self =>
  object Parser extends ParserBase with RationalParser with AgnosticLineEndingParser with JavaTokenParsers with OptionParser {
    def namesHeading = "Names" | "INDEX" | "INDICES" | "NAMES"
    def namesSection: Parser[HData] = ((namesHeading ~ lineEndings) ~> rep(ident)) ^^ { seq =>
      HData(names = Some(seq))
    }
    def variable: Parser[String] = ident
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

    //    def expr: Parser[Map[String, Rational]] =
    def constraint: Parser[NamedConstraint[Rational]] = expr ~ operator ~ expr ^^ {
      case lhs ~ op ~ rhs => NamedConstraint(None, lhs, op, rhs)
    }

    def mapRow: Parser[Seq[String]] = rep1(variable)
    def mapsSection: Parser[HData] = (("Maps" ~ lineEndings) ~> repsep(mapRow, lineEndings)) ^^ {
      seq => HData(maps = seq)
    }

    def constraintsHeading = "Equations" | "Inequalities" | "INEQUALITY_SECTION"
    def constraintsSection: Parser[HData] = ((constraintsHeading ~ lineEndings) ~> rep(constraint)) ^^ {
      seq => HData(constraints = seq)
    }

    def section: Parser[HData] = namesSection | constraintsSection | mapsSection

    def sections: Parser[HData] = rep1(section) into { secs =>
      (success(secs.head) /: secs.tail) {
        case (result, section) => result.flatMap { prevSection =>
          for {
            nextNames <- oneOptionOutOf(prevSection.names, section.names)
            nextConstraints = prevSection.constraints ++ section.constraints
            nextMaps = prevSection.maps ++ section.maps
          } yield HData(nextNames, nextConstraints, nextMaps)
        }
      }
    }

    def data = phrase(sections)
  }
}
