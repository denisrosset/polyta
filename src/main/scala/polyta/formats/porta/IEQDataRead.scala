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

trait IEQDataRead[M, V] extends FormatRead[IEQData[M, V]] { self =>
  implicit def M: MatVecInField[M, V, Rational]
  implicit def V: VecInField[V, Rational] = M.V

  object Parser extends ParserBase with PortaDataParser[M, V] {
    implicit def M = self.M
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
    def constraint(d: Int): Parser[Constraint[V]] =
      symbolicVector(d) ~ operator ~ rational ^^ {
        case lhs ~ op ~ rhs => Constraint(lhs, op, rhs)
      }
  }
}
