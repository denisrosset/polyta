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

final class IEQDataRead[M, V](implicit val alg: AlgMVF[M, V, Rational]) extends FormatRead[IEQData[M, V]] {

  object Parsers extends ParsersBase with PortaDataParsers[V] with ParsersUtils {

    def variable: Parser[Int] = ("x" ~> positiveInt).map(_ - 1)

    def symbolicVector(d: Int): Parser[V] =
      rationalCoefficientSignOptional ~ variable ~ rep(signedRationalCoefficient ~ variable) ^^ {
        case coeff0 ~ xInd0 ~ seq =>
          val varMap: Map[Int, Rational] = (Map(xInd0 -> coeff0) /: seq) {
            case (m, (coeff ~ xInd)) => m + (xInd -> (m.getOrElse(xInd, Rational.zero) + coeff))
          }
          V.tabulate(d)( k => varMap.getOrElse(k, Rational.zero) )
      }

    def operator: Parser[ComparisonOperator] = ("<=" ^^^ LE) | ("==" ^^^ EQ) | (">=" ^^^ GE)

    def constraint(d: Int): Parser[LinearConstraint[V, Rational]] =
      opt(lineNumber) ~> (symbolicVector(d) ~ operator ~ rational) ^^ {
        case lhs ~ op ~ rhs => LinearConstraint(lhs, op, rhs)
      }

    def constraintSection(d: Int): Parser[IEQData[M, V]] =
      (("INEQUALITIES_SECTION" ~ lineEndings) ~> repsep(constraint(d), lineEndings)) ^^ {
        constraints =>
        val eqs = constraints.collect {
          case lc: LinearEquality[V, Rational] => lc
        }
        val ineqs = constraints.collect {
          case lc: LinearInequality[V, Rational] => lc
        }
        val ineqRows = ineqs.map {
          case LinearInequalityLE(vec, _) => vec
          case LinearInequalityGE(vec, _) => -vec
        }
        val mA = MatBuilder[M, Rational].fromRows(d, ineqRows: _*)
        val vb = VecBuilder[V, Rational].build(ineqs.map {
          case LinearInequalityLE(_, r) => r
          case LinearInequalityGE(_, r) => -r
        }: _*)
        val eqRows = eqs.map(_.lhs)
        val mAeq = MatBuilder[M, Rational].fromRows(d, eqRows: _*)
        val vbeq = VecBuilder[V, Rational].build(eqs.map(_.rhs): _*)
        IEQData(polyhedron = HPolyhedronM(mA, vb, mAeq, vbeq))
      }

    def validSection(d: Int): Parser[IEQData[M, V]] =
      (("VALID" ~ lineEndings) ~> rowVector(d)) ^^ {
        v => IEQData.empty[M, V](d).copy(validPoint = Some(v))
      }

    def lowerBoundSection(d: Int): Parser[IEQData[M, V]] =
      (("LOWER_BOUNDS" ~ lineEndings) ~> rowVector(d)) ^^ {
        lb => IEQData.empty[M, V](d).copy(lowerBounds = Some(lb))
      }

    def upperBoundSection(d: Int): Parser[IEQData[M, V]] =
      (("UPPER_BOUNDS" ~ lineEndings) ~> rowVector(d)) ^^ {
        ub => IEQData.empty[M, V](d).copy(upperBounds = Some(ub))
      }

    def eliminationOrderSection(d: Int): Parser[IEQData[M, V]] =
      (("ELIMINATION_ORDER" ~ lineEndings) ~> rep(nonNegativeInt)) into { orderSeq =>
        if (orderSeq.size == d) {
          val eliminationOrder = orderSeq.zipWithIndex.filterNot(_._1 == 0).sortBy(_._1).map(_._2)
          success(IEQData.empty[M, V](d).copy(eliminationOrder = Some(eliminationOrder)))
        } else failure(s"ELIMINATION_ORDER should have $d elements, but has ${orderSeq.size}")
      }

    def section(d: Int): Parser[IEQData[M, V]] = constraintSection(d) | validSection(d) | lowerBoundSection(d) | upperBoundSection(d) | eliminationOrderSection(d)

    def sections(d: Int): Parser[IEQData[M, V]] = rep1(section(d) <~ lineEndings) into { secs =>
      (success(secs.head) /: secs.tail) {
        case (result, section) => result.flatMap { prevSection =>
          for {
            nextValidPoint <- oneOptionOutOf(prevSection.validPoint, section.validPoint)
            nextEliminationOrder <- oneOptionOutOf(prevSection.eliminationOrder, section.eliminationOrder)
            nextLowerBounds <- oneOptionOutOf(prevSection.lowerBounds, section.lowerBounds)
            nextUpperBounds <- oneOptionOutOf(prevSection.upperBounds, section.upperBounds)
            nextPolyhedron = HPolyhedronM.intersection(prevSection.polyhedron, section.polyhedron)
          } yield IEQData[M, V](nextPolyhedron, nextValidPoint, nextEliminationOrder, nextLowerBounds, nextUpperBounds)
        }
      }
    }

    def data: Parser[IEQData[M, V]] = (dimSection <~ lineEndings) into { d => sections(d) <~ end }
  }
}
