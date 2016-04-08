package com.faacets
package polyta
package formats
package qsopt

import scala.{specialized => sp}

import scala.util.parsing.combinator._

import scala.collection.BitSet

import spire.algebra._
import spire.math.Rational
import spire.syntax.order._
import spire.syntax.vectorSpace._
import spire.syntax.cfor._
import spire.util._

import SolverStatus._

class SolDataRead extends FormatRead[SolData] {

  object Parsers extends ParsersBase with AgnosticLineEndingParsers with RationalParsers {

    override val skipWhitespace = true
    override val whiteSpace = """([ \t])+""".r

    def optimal = "OPTIMAL" ^^^ OptimumFound
    def infeasible = "INFEASIBLE" ^^^ Infeasible
    def unbounded = "UNBOUNDED" ^^^ Unbounded
    def unsolved = ("UNDEFINED" | "NOT_SOLVED") ^^ { SolverFailure[String](_) }
    def status: Parser[SolverStatus[String]] = optimal | infeasible | unbounded | unsolved
    def firstStatusLine = (("status" ~ "=") ~> status) <~ lineEnding
    def secondStatusLine = ("status" ~> status) <~ lineEnding
    def valueLine: Parser[Rational] = (("Value" ~ "=") ~> rational) <~ lineEnding
    def name = "[a-zA-Z][a-zA-Z0-9]*".r
    def varLine: Parser[(String, Rational)] = (name <~ "=") ~ rational ^^ {
      case n ~ v => (n, v)
    }
    def varLines: Parser[Map[String, Rational]] = rep(varLine <~ lineEnding) ^^ (_.toMap)
    def varsSection: Parser[Map[String, Rational]] = ("VARS:" ~ lineEnding) ~> varLines
    def reducedCostSection: Parser[Map[String, Rational]] = ("REDUCED COST:" ~ lineEnding) ~> varLines
    def piSection: Parser[Map[String, Rational]] = ("PI:" ~ lineEnding) ~> varLines
    def slackSection: Parser[Map[String, Rational]] = ("SLACK:" ~ lineEnding) ~> varLines

    def data: Parser[SolData] = firstStatusLine into {
      case OptimumFound => secondStatusLine into {
        case OptimumFound => (valueLine ~ varsSection ~ reducedCostSection ~ piSection ~ slackSection) ^^ {
          case vl ~ vr ~ rc ~ pi ~ sl => SolData(OptimumFound, Some(vl), vr, rc, pi, sl)
        }
        case _ => failure("Inconsistent output")
      }
      case status => success(SolData(status))
    }

  }

}
