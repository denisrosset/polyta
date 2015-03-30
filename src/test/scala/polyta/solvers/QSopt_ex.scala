package com.faacets
package polyta
package solvers
package qsopt_ex

import org.scalatest.{FunSuite, NonImplicitAssertions, Matchers}

import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra._
import spire.math._
import spire.compat._
import spire.syntax.all._
import spire.std.any._
import spire.util._

import qalg.algebra._
import qalg.math._

class QSopt_exSuite extends FunSuite with NonImplicitAssertions with Matchers {
  type M = DenseM[Rational]
  type V = DenseV[Rational]
  type A = Rational
  val V = VecInField[V, A]
  trait DenseRationalBuilder extends LinearProgramBuilder[M, V, A] {
    def MV = MatVecInField[M, V, A]
  }
  test("Problem 1") {
    object Problem extends DenseRationalBuilder {
      val x1 = Var("x1")
      val x2 = Var("x2")
      minimize(2 *: x1 + x2)
      subjectTo(
        -x1 + x2 <= 1,
        x1 + x2 >= 2,
        x2 >= 0,
        x1 - 2 *: x2 <= 4
      )
    }
    val solution = QSopt_ex[M, V].solveLP(Problem.result)
    solution.optimalValue shouldBe(Opt(Rational(5,2)))
    solution.optimalSolution shouldBe(Opt(V.build(Rational(1,2),Rational(3,2))))
  }
  test("Problem 2") {
    object Problem extends DenseRationalBuilder {
      val x1 = Var("x1")
      val x2 = Var("x2")
      maximize(x1 + x2)
      subjectTo(
        2 *: x1 + 3 *: x2 <= 9,
        2 *: x1 + x2 <= 5,
        x1 >= 0,
        x2 >= 0
      )
    }
    val solution = QSopt_ex[M, V].solveLP(Problem.result)
    solution.optimalValue shouldBe(Opt(Rational(7, 2)))
    solution.optimalSolution shouldBe(Opt(V.build(Rational(3,2), 2)))
  }
}
