package com.faacets
package polyta
package solvers
package reference

import scala.annotation.tailrec
import scala.{specialized => sp}
import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra._
import spire.math._
import spire.syntax.all._
import spire.std.any._

sealed trait SimplexResult
case object SimplexUnbounded extends SimplexResult
case object SimplexOptimal extends SimplexResult
case object SimplexError extends SimplexResult

/** Taken from Sedgewick, Algorithms, 4th Edition code.
  * Given an M-by-N matrix A, an M-length vector b, and an
  * N-length vector c, solve the LP { max cx : Ax <= b, x >= 0 }.
  * Assumes that b >= 0 so that x = 0 is a basic feasible solution.
  *
  * Creates an (M+1)-by-(N+M+1) simplex tableaux with the
  * RHS in column M+N, the objective function in row M, and
  * slack variables in columns M through M+N-1.
  */
class Simplex[@sp(Double) T:Field:Order:ClassTag](val A: Array[Array[T]], val b: Array[T], val c: Array[T], epsilon: T) {
  @inline def zero = Field[T].zero
  @inline def one = Field[T].one
  @inline def M: Int = b.length
  @inline def N: Int = c.length
  val a: Array[Array[T]] = Array.tabulate[T](M + 1, N + M + 1) { (i, j) =>
    if (j < N) {
      if (i < M) A(i)(j) else c(j)
    }
    else if (j < N + M) {
      if (j == N + i) one else zero
    }
    else {
      if (i < M) b(i) else zero
    }
  }
  val basis: Array[Int] = Array.tabulate[Int](M)((i: Int) => i + N)

  def solve(): SimplexResult = {
    // returns true if unbounded
    @tailrec def iterate(): Boolean = {
      val q = bland()
      if (q == -1) return false
      val p = minRatioRule(q)
      if (p == -1) return true
      pivot(p, q)
      basis(p) = q
      iterate()
    }
    if (iterate()) return SimplexUnbounded
    if (check(A, b, c)) SimplexOptimal else SimplexError
  }

  private def bland(): Int = {
    cforRange(0 until M + N){ j =>
      if (a(M)(j) > zero) return j
    }
    -1
  }
  private def minRatioRule(q: Int): Int = {
    var p = -1
    cforRange(0 until M) { i =>
      if (a(i)(q) <= zero) { }
      else if (p == -1) p = i
      else if ((a(i)(M + N) / a(i)(q)) < (a(p)(M + N) / a(p)(q))) p = i
    }
    p
  }

  private def pivot(p: Int, q: Int): Unit = {
    cforRange(0 to M) { i =>
      cforRange(0 to M + N) { j =>
        if (i != p && j != q) a(i)(j) -= a(p)(j) * a(i)(q) / a(p)(q)
      }
    }
    cforRange(0 to M) { i =>
      if (i != p) a(i)(q) = zero
    }
    cforRange(0 to M + N) { j =>
      if (j != q) a(p)(j) /= a(p)(q)
    }
    a(p)(q) = one
  }

  private def check(A: Array[Array[T]], b: Array[T], c: Array[T]): Boolean = {
    isPrimalFeasible(A, b) && isDualFeasible(A, c) && isOptimal(b, c)
  }

  private def isPrimalFeasible(A: Array[Array[T]], b: Array[T]): Boolean = {
    val x: Array[T] = primal
    cforRange(0 until x.length) { j =>
      if (x(j) < zero) {
        println("x[" + j + "] = " + x(j) + " is negative")
        return false
      }
    }
    cforRange(0 until M) { i =>
      var sum: T = zero
      cforRange(0 until N) { j =>
        sum += A(i)(j) * x(j)
      }
      if (sum > b(i) + epsilon) {
        println("not primal feasible")
        println("b[" + i + "] = " + b(i) + ", sum = " + sum)
        return false
      }
    }
    true
  }

  def primal: Array[T] = {
    val x: Array[T] = new Array[T](N)
    cforRange(0 until M) { i =>
      if (basis(i) < N) x(basis(i)) = a(i)(M + N)
    }
    x
  }

  private def isDualFeasible(A: Array[Array[T]], c: Array[T]): Boolean = {
    val y: Array[T] = dual
    cforRange(0 until y.length) { i =>
      if (y(i) < zero) {
        println("y[" + i + "] = " + y(i) + " is negative")
        return false
      }
    }
    cforRange(0 until N) { j =>
      var sum: T = zero
      cforRange(0 until M) { i =>
        sum += A(i)(j) * y(i)
      }
      if (sum < c(j) - epsilon) {
        println("not dual feasible")
        println("c[" + j + "] = " + c(j) + ", sum = " + sum)
        return false
      }
    }
    true
  }

  def dual: Array[T] = {
    val y: Array[T] = new Array[T](M)
    cforRange(0 until M) { i =>
      y(i) = -a(M)(N + i)
    }
    y
  }

  private def isOptimal(b: Array[T], c: Array[T]): Boolean = {
    val x: Array[T] = primal
    val y: Array[T] = dual
    val valueComp: T = value
    var value1: T = zero
    cforRange(0 until x.length) { j =>
      value1 += c(j) * x(j)
    }
    var value2: T = zero
    cforRange(0 until y.length) { i =>
     value2 += y(i) * b(i)
    }
    if ((valueComp - value1).abs > epsilon || (valueComp - value2).abs > epsilon) {
      println("value = " + valueComp + ", cx = " + value1 + ", yb = " + value2)
      false
    } else true
  }

  def show(): Unit = {
    println("M = " + M)
    println("N = " + N)
    cforRange(0 to M) { i =>
      cforRange(0 until M + N) { j =>
        print(a(i)(j) + " ")
      }
      println
    }
    println("value = " + value)
    cforRange(0 until M) { i =>
      if (basis(i) < N) println("x_" + basis(i) + " = " + a(i)(M + N))
    }
    println()
  }

  def value: T = -a(M)(M + N)

  private def dantzig: Int = {
    var q = 0
    cforRange(1 until M + N) { j =>
      if (a(M)(j) > a(M)(q)) q = j
    }
    if (a(M)(q) <= 0) -1 else q
  }
}
