package com.faacets
package polyta
package formats
package qsopt

import spire.math.Rational

import SolverStatus._
import fastparse.WhitespaceApi

object Qsopt {

  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(CharsWhile(" \t".contains(_)).?)
  }

}

class SolDataRead extends FormatRead[SolData] with RationalParsers with AgnosticLineEndingParsers {

  import fastparse.noApi._

  implicit def parserApi[T, V](p0: T)(implicit c: T => fastparse.all.P[V]): WhitespaceApi[V] =
    Qsopt.White.parserApi(p0)(c)

  val optimal = P("OPTIMAL").map( x => OptimumFound )
  val infeasible = P("INFEASIBLE").map( x => Infeasible )
  val unbounded = P("UNBOUNDED").map( x => Unbounded )
  val unsolved = P("UNDEFINED".! | "NOT_SOLVED".!).map( SolverFailure[String](_) )
  val status: P[SolverStatus[String]] = P( optimal | infeasible | unbounded | unsolved )
  val firstStatusLine = P("status" ~ "=" ~ status ~ lineEnding)
  val secondStatusLine = P("status" ~ status ~ lineEnding)
  val valueLine: P[Rational] = P("Value" ~/ "=" ~ rational ~ lineEnding)

  val name = P( (CharPred( c => c.isLetter ) ~~ CharsWhile(c => c.isLetter || c.isDigit )).! )

  val varLine: P[(String, Rational)] = P(name ~ "=" ~ rational)
  val varLines: P[Map[String, Rational]] = P( varLine ~ lineEnding ).rep.map(_.toMap)
  val varsSection: P[Map[String, Rational]] = P( "VARS:" ~/ lineEnding ~ varLines )
  val reducedCostSection: P[Map[String, Rational]] = P( "REDUCED COST:" ~/ lineEnding ~ varLines )
  val piSection: P[Map[String, Rational]] = P( "PI:" ~ lineEnding ~ varLines )
  val slackSection: P[Map[String, Rational]] = P( "SLACK:" ~ lineEnding ~ varLines )

  val data: P[SolData] = firstStatusLine.flatMap {
      case OptimumFound => (Pass ~ secondStatusLine).flatMap {
        case OptimumFound => (Pass ~ valueLine ~ varsSection ~ reducedCostSection ~ piSection ~ slackSection).map {
          case (vl, vr, rc, pi, sl) => SolData(OptimumFound, Some(vl), vr, rc, pi, sl)
        }
        case _ => throw new IllegalArgumentException("Inconsistent output")
      }
      case status => Pass.map( x => SolData(status) )
    }

}
