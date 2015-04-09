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

final class IEQDataRead[M, V](implicit val M: MatVecInField[M, V, Rational]) extends FormatRead[IEQData[M, V]] {
  implicit def V: VecInField[V, Rational] = M.V

  object Parser extends ParserBase with PortaDataParser[V] with ParserUtils {
    implicit def V = IEQDataRead.this.V

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
        val ineqRows = M.zeros(0, d) +: ineqs.map {
          case LinearInequalityLE(vec, _) => vec.rowMat[M]
          case LinearInequalityGE(vec, _) => (-vec).rowMat[M]
        }
        val mA = M.vertcat(ineqRows: _*)
        val vb = V.build(ineqs.map {
          case LinearInequalityLE(_, r) => r
          case LinearInequalityGE(_, r) => -r
        }: _*)
        val eqRows = M.zeros(0, d) +: eqs.map( c => c.lhs.rowMat[M] )
        val mAeq = M.vertcat(eqRows: _*)
        val vbeq = V.build(eqs.map(_.rhs): _*)
        IEQData(polyhedron = HPolyhedron(mA, vb, mAeq, vbeq))
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
            nextPolyhedron = HPolyhedron.intersection(prevSection.polyhedron, section.polyhedron)
          } yield IEQData[M, V](nextPolyhedron, nextValidPoint, nextEliminationOrder, nextLowerBounds, nextUpperBounds)
        }
      }
    }

    def data: Parser[IEQData[M, V]] = (dimSection <~ lineEndings) into { d => sections(d) <~ end }
  }
}
