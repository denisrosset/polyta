package com.faacets
package polyta
package solvers
package reference

import org.scalatest.{FunSuite, NonImplicitAssertions, Matchers}

import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra._
import spire.math._
import spire.compat._
import spire.syntax.all._
import spire.std.any._

class SimplexSedgewickAlgorithmsSuite extends FunSuite with NonImplicitAssertions with Matchers {
  def norm2[T:Field](a: Array[T]): T = (Field[T].zero /: a) { case (acc, c) => acc + c*c }

  def testOptimal[T:Field:Order:ClassTag](A: Array[Array[T]], b: Array[T], c: Array[T],
    expectedValue: T, expectedX: Array[T], expectedY: Array[T], epsilon: T, tol: T): Unit = {
    val tol2 = tol * tol
    val lp = new Simplex[T](A, b, c, epsilon)
    lp.solve shouldBe SimplexOptimal
    val x = lp.primal
    val y = lp.dual
    val value = lp.value
    norm2(x - expectedX) should be <= tol2
    norm2(y - expectedY) should be <= tol2
      (value - expectedValue).abs should be <= tol
  }

  def testUnbounded[T:Field:Order:ClassTag](A: Array[Array[T]], b: Array[T], c: Array[T], epsilon: T): Unit = {
    val lp = new Simplex[T](A, b, c, epsilon)
    lp.solve shouldBe SimplexUnbounded
  }

  test("Test 1, double") {
    val A: Array[Array[Double]] = Array(Array(-1, 1, 0), Array(1, 4, 0), Array(2, 1, 0), Array(3, -4, 0), Array(0, 0, 1))
    val c: Array[Double] = Array(1, 1, 1)
    val b: Array[Double] = Array(5, 45, 27, 24, 4)
    val expectedValue: Double = 22.0
    val expectedX: Array[Double] = Array(9, 9, 4)
    val expectedY: Array[Double] = Array(0.0, 1.0/7, 3.0/7, 0.0, 1.0)
    val epsilon = 1.0e-10
    val tol = 1.0e-10
    testOptimal[Double](A, b, c, expectedValue, expectedX, expectedY, epsilon, tol)
  }
  test("Test 1, rational") {
    val A: Array[Array[Rational]] = Array(Array(-1, 1, 0), Array(1, 4, 0), Array(2, 1, 0), Array(3, -4, 0), Array(0, 0, 1))
    val c: Array[Rational] = Array(1, 1, 1)
    val b: Array[Rational] = Array(5, 45, 27, 24, 4)
    val expectedValue: Rational = 22
    val expectedX: Array[Rational] = Array(9, 9, 4)
    val expectedY: Array[Rational] = Array(Rational.zero, Rational(1, 7), Rational(3, 7), Rational.zero, Rational.one)
    val epsilon = Rational.zero
    val tol = Rational.zero
    testOptimal[Rational](A, b, c, expectedValue, expectedX, expectedY, epsilon, tol)
  }
  test("Test 2, double") {
    val A: Array[Array[Double]] = Array(Array(5.0, 15.0), Array(4.0, 4.0), Array(35.0, 20.0))
    val b: Array[Double] = Array(480.0, 160.0, 1190.0)
    val c: Array[Double] = Array(13.0, 23.0)
    val expectedValue: Double = 800.0
    val expectedX: Array[Double] = Array(12.0, 28.0)
    val expectedY: Array[Double] = Array(1.0, 2.0, 0.0)
    val epsilon = 1.0e-10
    val tol = 1.0e-10
    testOptimal[Double](A, b, c, expectedValue, expectedX, expectedY, epsilon, tol)
  }
  test("Test 2, rational") {
    val A: Array[Array[Rational]] = Array(Array(5, 15), Array(4, 4), Array(35, 20))
    val b: Array[Rational] = Array(480, 160, 1190)
    val c: Array[Rational] = Array(13, 23)
    val expectedValue: Rational = 800
    val expectedX: Array[Rational] = Array(12, 28)
    val expectedY: Array[Rational] = Array(1, 2, 0)
    val epsilon = Rational.zero
    val tol = Rational.zero
    testOptimal[Rational](A, b, c, expectedValue, expectedX, expectedY, epsilon, tol)
  }
  test("Test 3, double") {
    val A: Array[Array[Double]] = Array(Array(-2.0, -9.0, 1.0, 9.0), Array(1.0, 1.0, -1.0, -2.0))
    val b: Array[Double] = Array(3.0, 2.0)
    val c: Array[Double] = Array(2.0, 3.0, -1.0, -12.0)
    val epsilon = 1.0e-10
    testUnbounded[Double](A, b, c, epsilon)
  }
  test("Test 4, double") {
    val A: Array[Array[Double]] = Array(Array(0.5, -5.5, -2.5, 9.0), Array(0.5, -1.5, -0.5, 1.0), Array(1.0, 0.0, 0.0, 0.0))
    val b: Array[Double] = Array(0.0, 0.0, 1.0)
    val c: Array[Double] = Array(10.0, -57.0, -9.0, -24.0)
    val epsilon = 1.0e-10
    val tol = 1.0e-10
    val expectedValue = 1.0
    val expectedX = Array(1.0, 0.0, 1.0, 0.0)
    val expectedY = Array(0.0, 18.0, 1.0)
    testOptimal[Double](A, b, c, expectedValue, expectedX, expectedY, epsilon, tol)
  }
}
