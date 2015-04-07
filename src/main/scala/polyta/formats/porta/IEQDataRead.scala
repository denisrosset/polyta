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

import qalg.algebra._
import qalg.algos._
import qalg.math._
import qalg.syntax.all._

trait IEQDataRead[V] extends FormatRead[IEQData[V]] { self =>
  implicit def V: VecInField[V, Rational]

  object Parser extends ParserBase with PortaDataParser[V] with ParserUtils {
    implicit def V = self.V

    def variable: Parser[Int] = ("x" ~> positiveInt).map(_ - 1)

    def symbolicVector(d: Int): Parser[V] =
      rationalCoefficientSignOptional ~ variable ~ rep(signedRationalCoefficient ~ variable) ^^ {
        case coeff0 ~ xInd0 ~ seq =>
          val varMap: Map[Int, Rational] = (Map(xInd0 -> coeff0) /: seq) {
            case (m, (coeff ~ xInd)) => m + (xInd -> (m.getOrElse(xInd, Rational.zero) + coeff))
          }
          V.fromFunV(new FunV[Rational] {
            def len = d
            def f(k: Int): Rational = varMap.getOrElse(k, Rational.zero)
          })
      }

    def operator: Parser[ComparisonOperator] = ("<=" ^^^ LE) | ("==" ^^^ EQ) | (">=" ^^^ GE)

    def constraint(d: Int): Parser[VConstraint[V, Rational]] =
      opt(lineNumber) ~> (symbolicVector(d) ~ operator ~ rational) ^^ {
        case lhs ~ op ~ rhs => VConstraint(lhs, op, rhs)
      }

    def constraintSection(d: Int): Parser[IEQData[V]] =
      (("INEQUALITIES_SECTION" ~ lineEndings) ~> repsep(constraint(d), lineEndings)) ^^ {
        constraints => IEQData(d, constraints, None, None, None, None)
      }

    def validSection(d: Int): Parser[IEQData[V]] =
      (("VALID" ~ lineEndings) ~> rowVector(d)) ^^ {
        v => IEQData(d, Seq.empty, Some(v), None, None, None)
      }

    def lowerBoundSection(d: Int): Parser[IEQData[V]] =
      (("LOWER_BOUNDS" ~ lineEndings) ~> rowVector(d)) ^^ {
        lb => IEQData(d, Seq.empty, None, None, Some(lb), None)
      }

    def upperBoundSection(d: Int): Parser[IEQData[V]] =
      (("UPPER_BOUNDS" ~ lineEndings) ~> rowVector(d)) ^^ {
        ub => IEQData(d, Seq.empty, None, None, None, Some(ub))
      }

    def eliminationOrderSection(d: Int): Parser[IEQData[V]] =
      (("ELIMINATION_ORDER" ~ lineEndings) ~> rep(nonNegativeInt)) into { orderSeq =>
        if (orderSeq.size == d) {
          val eliminationOrder = orderSeq.zipWithIndex.filterNot(_._1 == 0).sortBy(_._1).map(_._2)
          success(IEQData(d, Seq.empty, None, Some(eliminationOrder), None, None))
        } else failure(s"ELIMINATION_ORDER should have $d elements, but has ${orderSeq.size}")
      }

    def section(d: Int): Parser[IEQData[V]] = constraintSection(d) | validSection(d) | lowerBoundSection(d) | upperBoundSection(d) | eliminationOrderSection(d)

    def sections(d: Int): Parser[IEQData[V]] = rep1(section(d) <~ lineEndings) into { secs =>
      (success(secs.head) /: secs.tail) {
        case (result, section) => result.flatMap { prevSection =>
          for {
            nextValidPoint <- oneOptionOutOf(prevSection.validPoint, section.validPoint)
            nextEliminationOrder <- oneOptionOutOf(prevSection.eliminationOrder, section.eliminationOrder)
            nextLowerBounds <- oneOptionOutOf(prevSection.lowerBounds, section.lowerBounds)
            nextUpperBounds <- oneOptionOutOf(prevSection.upperBounds, section.upperBounds)
            nextConstraints = prevSection.constraints ++ section.constraints
          } yield IEQData(d, nextConstraints, nextValidPoint, nextEliminationOrder, nextLowerBounds, nextUpperBounds)
        }
      }
    }

    def data: Parser[IEQData[V]] = (dimSection <~ lineEndings) into { d => sections(d) <~ end }
  }
}
