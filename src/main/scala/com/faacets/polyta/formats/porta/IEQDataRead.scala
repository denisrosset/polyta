package com.faacets
package polyta
package formats
package porta

import spire.math.Rational

import scalin.immutable.Vec

import scalin.immutable.dense._
import scalin.syntax.build.{tabulate, zeros}

import fastparse.noApi._

import ComparisonOp._
final class IEQDataRead extends FormatRead[IEQData] with PortaDataParsers {

  sealed trait Section

  case class LowerBound(lb: Vec[Rational]) extends Section

  case class UpperBound(ub: Vec[Rational]) extends Section

  case class EliminationOrder(eo: Seq[Int]) extends Section

  case class Inequalities(poly: HPolytope[Rational]) extends Section

  case class ValidPoint(x: Vec[Rational]) extends Section

  val variable: P[Int] = P( "x" ~ positiveInt ).map(_ - 1)

  def symbolicVector(d: Int): P[Vec[Rational]] =
    P( rationalCoefficientSignOptional ~ variable ~ (signedRationalCoefficient ~ variable).rep ).map {
      case (coeff0, xInd0, seq) =>
        val varMap: Map[Int, Rational] = (Map(xInd0 -> coeff0) /: seq) {
          case (m, (coeff, xInd)) => m + (xInd -> (m.getOrElse(xInd, Rational.zero) + coeff))
        }
        tabulate(d)( k => varMap.getOrElse(k, Rational.zero) )
    }

  val operator: P[ComparisonOp] = P("<=").map(x => LE) | P("==").map(x => EQ) | P(">=").map(x => GE)

  def constraint(d: Int): Parser[LinearConstraint[Rational]] =
    P( lineNumberForget.? ~ symbolicVector(d) ~ operator ~ rational ).map {
      case (lhs, op, rhs) => LinearConstraint(lhs, op, rhs)
    }

  def inequalitiesSection(d: Int): P[Inequalities] =
    P( "INEQUALITIES_SECTION" ~/ lineEndings ~ constraint(d).rep(sep = lineEndings) ).map {
      constraints =>
        val eqs = constraints.collect {
          case lc: LinearEquality[Rational] => lc
        }
        val ineqs = constraints.collect {
          case lc: LinearInequality[Rational] => lc
        }
        val mA = tabulate(ineqs.size, d)( (r, c) =>
          if (ineqs(r).op == LE) ineqs(r).lhs(c) else -ineqs(r).lhs(c)
        )
        val vb = tabulate(ineqs.size)( i => if (ineqs(i).op == LE) ineqs(i).rhs else -ineqs(i).rhs )
        val mAeq = tabulate(eqs.size, d)( (r, c) => eqs(r).lhs(c) )
        val vbeq = tabulate(eqs.size)( i => eqs(i).rhs )
        Inequalities(HPolytope(mA, vb, mAeq, vbeq))
    }

  def validSection(d: Int): P[ValidPoint] =
    P( "VALID" ~/ lineEndings ~ rowVector(d) ).map {
      v => ValidPoint(v)
    }


  def lowerBoundSection(d: Int): P[LowerBound] =
    P( "LOWER_BOUNDS" ~/ lineEndings ~ rowVector(d) ).map {
      lb => LowerBound(lb)
    }

  def upperBoundSection(d: Int): P[UpperBound] =
    P( "UPPER_BOUNDS" ~ lineEndings ~ rowVector(d) ).map {
      ub => UpperBound(ub)
    }

  def eliminationOrderSection(d: Int): P[EliminationOrder] =
    P( "ELIMINATION_ORDER" ~ lineEndings ~ nonNegativeInt.rep ).filter(_.size == d).map { orderSeq =>
      val eliminationOrder = orderSeq.zipWithIndex.filterNot(_._1 == 0).sortBy(_._1).map(_._2)
      EliminationOrder(eliminationOrder)
    }

  def section(d: Int): P[Section] =
    inequalitiesSection(d) | validSection(d) | lowerBoundSection(d) | upperBoundSection(d) | eliminationOrderSection(d)

  def sections(d: Int): P[IEQData] = P( section(d) ~ lineEndings ).rep(min = 1)
    .filter( _.count(_.isInstanceOf[LowerBound]) <= 1 )
    .filter( _.count(_.isInstanceOf[UpperBound]) <= 1 )
    .filter( _.count(_.isInstanceOf[ValidPoint]) <= 1 )
    .filter( _.count(_.isInstanceOf[EliminationOrder]) <= 1)
    .filter( _.count(_.isInstanceOf[Inequalities]) <= 1 )
    .map { sections =>
      val lowerBounds = sections.collect { case sec: LowerBound => sec.lb }.headOption
      val upperBounds = sections.collect { case sec: UpperBound => sec.ub }.headOption
      val validPoint = sections.collect { case sec: ValidPoint => sec.x }.headOption.getOrElse(zeros[Rational](d))
      val polytope = sections.collect { case sec: Inequalities => sec.poly }.headOption.getOrElse(HPolytope.full[Rational](d))
      val eliminationOrder = sections.collect { case sec: EliminationOrder => sec.eo }.headOption
      IEQData(polytope, validPoint, eliminationOrder, lowerBounds, upperBounds)
    }

  def data: Parser[IEQData] = P( dimSection ~ lineEndings ).flatMap { d => sections(d) ~ end }

}
