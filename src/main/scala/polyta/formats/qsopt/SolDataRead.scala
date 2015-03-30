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

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

import solvers._

trait SolDataRead[A] extends FormatRead[SolData[A]] {
  trait SolDataParser extends ParserBase with AgnosticLineEndingParser {
    override val skipWhitespace = true
    override val whiteSpace = """([ \t])+""".r

    def value: Parser[A]

    def optimal = "OPTIMAL" ^^^ OptimumFound
    def infeasible = "INFEASIBLE" ^^^ Infeasible
    def unbounded = "UNBOUNDED" ^^^ Unbounded
    def unsolved = ("UNDEFINED" | "NOT_SOLVED") ^^^ CouldNotSolve
    def status = optimal | infeasible | unbounded | unsolved
    def firstStatusLine = (("status" ~ "=") ~> status) <~ crlf
    def secondStatusLine = ("status" ~> status) <~ crlf
    def valueLine: Parser[A] = (("Value" ~ "=") ~> value) <~ crlf
    def name = "[a-zA-Z][a-zA-Z0-9]*".r
    def varLine: Parser[(String, A)] = (name <~ "=") ~ value ^^ {
      case n ~ v => (n, v)
    }
    def varLines: Parser[Map[String, A]] = rep(varLine <~ crlf) ^^ (_.toMap)
    def varsSection: Parser[Map[String, A]] = ("VARS:" ~ crlf) ~> varLines
    def reducedCostSection: Parser[Map[String, A]] = ("REDUCED COST:" ~ crlf) ~> varLines
    def piSection: Parser[Map[String, A]] = ("PI:" ~ crlf) ~> varLines
    def slackSection: Parser[Map[String, A]] = ("SLACK:" ~ crlf) ~> varLines

    def data: Parser[SolData[A]] = firstStatusLine into {
      case OptimumFound => secondStatusLine into {
        case OptimumFound => (valueLine ~ varsSection ~ reducedCostSection ~ piSection ~ slackSection) ^^ {
          case vl ~ vr ~ rc ~ pi ~ sl => SolData(OptimumFound, Some(vl), vr, rc, pi, sl)
        }
        case _ => failure("Inconsistent output")
      }
      case status => success(SolData[A](status))
    }
  }
}
