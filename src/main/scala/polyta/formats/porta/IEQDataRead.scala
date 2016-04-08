package com.faacets
package polyta
package formats
package porta

import java.io.{Reader, Writer}

import scala.{specialized => sp}

import scala.collection.{BitSet, SortedSet}

import spire.algebra._
import spire.math.Rational
import spire.syntax.order._
import spire.syntax.vectorSpace._
import spire.syntax.cfor._
import spire.util._

import scalin.immutable.dense._
import scalin.immutable.{DenseMat => IMat, DenseVec => IVec}
import scalin.syntax.all._

import ComparisonOp._

final class IEQDataRead extends FormatRead[IEQData] {

  object Parsers extends ParsersBase with PortaDataParsers with ParsersUtils {

    def variable: Parser[Int] = ("x" ~> positiveInt).map(_ - 1)

    def symbolicVector(d: Int): Parser[IVec[Rational]] =
      rationalCoefficientSignOptional ~ variable ~ rep(signedRationalCoefficient ~ variable) ^^ {
        case coeff0 ~ xInd0 ~ seq =>
          val varMap: Map[Int, Rational] = (Map(xInd0 -> coeff0) /: seq) {
            case (m, (coeff ~ xInd)) => m + (xInd -> (m.getOrElse(xInd, Rational.zero) + coeff))
          }
          IVec.tabulate(d)( k => varMap.getOrElse(k, Rational.zero) )
      }

    def operator: Parser[ComparisonOp] = ("<=" ^^^ LE) | ("==" ^^^ EQ) | (">=" ^^^ GE)

    def constraint(d: Int): Parser[LinearConstraint[Rational]] =
      opt(lineNumber) ~> (symbolicVector(d) ~ operator ~ rational) ^^ {
        case lhs ~ op ~ rhs => LinearConstraint(lhs, op, rhs)
      }

    def constraintSection(d: Int): Parser[IEQData] =
      (("INEQUALITIES_SECTION" ~ lineEndings) ~> repsep(constraint(d), lineEndings)) ^^ {
        constraints =>
        val eqs = constraints.collect {
          case lc: LinearEquality[Rational] => lc
        }
        val ineqs = constraints.collect {
          case lc: LinearInequality[Rational] => lc
        }
        val mA = IMat.tabulate(ineqs.size, d)( (r, c) =>
          if (ineqs(r).op == LE) ineqs(r).lhs(c) else -ineqs(r).lhs(c)
        )
        val vb = IVec.tabulate(ineqs.size)( i => if (ineqs(i).op == LE) ineqs(i).rhs else -ineqs(i).rhs )
        val mAeq = IMat.tabulate(eqs.size, d)( (r, c) => eqs(r).lhs(c) )
        val vbeq = IVec.tabulate(eqs.size)( i => eqs(i).rhs )
        IEQData(polytope = HPolytopeM(mA, vb, mAeq, vbeq))
      }

    def validSection(d: Int): Parser[IEQData] =
      (("VALID" ~ lineEndings) ~> rowVector(d)) ^^ {
        v => IEQData.empty(d).copy(validPoint = Some(v))
      }

    def lowerBoundSection(d: Int): Parser[IEQData] =
      (("LOWER_BOUNDS" ~ lineEndings) ~> rowVector(d)) ^^ {
        lb => IEQData.empty(d).copy(lowerBounds = Some(lb))
      }

    def upperBoundSection(d: Int): Parser[IEQData] =
      (("UPPER_BOUNDS" ~ lineEndings) ~> rowVector(d)) ^^ {
        ub => IEQData.empty(d).copy(upperBounds = Some(ub))
      }

    def eliminationOrderSection(d: Int): Parser[IEQData] =
      (("ELIMINATION_ORDER" ~ lineEndings) ~> rep(nonNegativeInt)) into { orderSeq =>
        if (orderSeq.size == d) {
          val eliminationOrder = orderSeq.zipWithIndex.filterNot(_._1 == 0).sortBy(_._1).map(_._2)
          success(IEQData.empty(d).copy(eliminationOrder = Some(eliminationOrder)))
        } else failure(s"ELIMINATION_ORDER should have $d elements, but has ${orderSeq.size}")
      }

    def section(d: Int): Parser[IEQData] = constraintSection(d) | validSection(d) | lowerBoundSection(d) | upperBoundSection(d) | eliminationOrderSection(d)

    def sections(d: Int): Parser[IEQData] = rep1(section(d) <~ lineEndings) into { secs =>
      (success(secs.head) /: secs.tail) {
        case (result, section) => result.flatMap { prevSection =>
          for {
            nextValidPoint <- oneOptionOutOf(prevSection.validPoint, section.validPoint)
            nextEliminationOrder <- oneOptionOutOf(prevSection.eliminationOrder, section.eliminationOrder)
            nextLowerBounds <- oneOptionOutOf(prevSection.lowerBounds, section.lowerBounds)
            nextUpperBounds <- oneOptionOutOf(prevSection.upperBounds, section.upperBounds)
            nextPolytope = intersection(prevSection.polytope, section.polytope)
          } yield IEQData(nextPolytope, nextValidPoint, nextEliminationOrder, nextLowerBounds, nextUpperBounds)
        }
      }
    }

    def data: Parser[IEQData] = (dimSection <~ lineEndings) into { d => sections(d) <~ end }
  }

}
