import scala.annotation.tailrec
import scala.{specialized => sp}
import scala.reflect.ClassTag
import spire.algebra._
import spire.math._
import spire.syntax.all._
import spire.std.any._

class Simplex[T:Field:Order:ClassTag](A: Array[Array[T]], b: Array[T], c: Array[T], epsilon: T) {
  @inline def zero = Field[T].zero
  @inline def one = Field[T].one
  val M: Int = b.length
  val N: Int = c.length
  val a: Array[Array[T]] = Array.fill[T](M + 1, N + M +1)(zero)
  val basis: Array[Int] = Array.tabulate[Int](M)((i: Int) => i + N)

  cforRange(0 until M) { i =>
    cforRange(0 until N) { j =>
      a(i)(j) = A(i)(j)
    }
  }
  cforRange(0 until M) { i =>
    a(i)(N + i) = one
  }
  cforRange(0 until N) { j =>
    a(M)(j) = c(j)
  }
  cforRange(0 until M) { i =>
    a(i)(M + N) = b(i)
  }
  solve()
  assert(check(A, b, c))

  private def solve(): Unit = {
    while (true) {
      val q: Int = bland
      if (q == -1) return
      val p: Int = minRatioRule(q)
      if (p == -1) throw new ArithmeticException("Linear program is unbounded")
      pivot(p, q)
      basis(p) = q
    }
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

  private def pivot(p: Int, q: Int) {
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
        StdOut.println("x[" + j + "] = " + x(j) + " is negative")
        return false
      }
    }
    cforRange(0 until M) { i =>
      var sum: T = zero
      cforRange(0 until N) { j =>
        sum += A(i)(j) * x(j)
      }
      if (sum > b(i) + epsilon) {
        StdOut.println("not primal feasible")
        StdOut.println("b[" + i + "] = " + b(i) + ", sum = " + sum)
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
        StdOut.println("y[" + i + "] = " + y(i) + " is negative")
        return false
      }
    }
    cforRange(0 until N) { j =>
      var sum: T = zero
      cforRange(0 until M) { i =>
        sum += A(i)(j) * y(i)
      }
      if (sum < c(j) - epsilon) {
        StdOut.println("not dual feasible")
        StdOut.println("c[" + j + "] = " + c(j) + ", sum = " + sum)
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
      StdOut.println("value = " + valueComp + ", cx = " + value1 + ", yb = " + value2)
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

/** ***********************************************************************
  * Compilation:  javac Simplex.java
  * Execution:    java Simplex
  *
  * Given an M-by-N matrix A, an M-length vector b, and an
  * N-length vector c, solve the  LP { max cx : Ax <= b, x >= 0 }.
  * Assumes that b >= 0 so that x = 0 is a basic feasible solution.
  *
  * Creates an (M+1)-by-(N+M+1) simplex tableaux with the
  * RHS in column M+N, the objective function in row M, and
  * slack variables in columns M through M+N-1.
  *
  * ************************************************************************/
object Simplex {
  private val EPSILON: Double = 1.0E-10

  def main(args: Array[String]) {
    try {
      test1
    }
    catch {
      case e: ArithmeticException => {
        e.printStackTrace
      }
    }
    StdOut.println("--------------------------------")
    try {
      test2
    }
    catch {
      case e: ArithmeticException => {
        e.printStackTrace
      }
    }
    StdOut.println("--------------------------------")
    try {
      test3
    }
    catch {
      case e: ArithmeticException => {
        e.printStackTrace
      }
    }
    StdOut.println("--------------------------------")
    try {
      test4
    }
    catch {
      case e: ArithmeticException => {
        e.printStackTrace
      }
    }
    StdOut.println("--------------------------------")
    val M: Int = args(0).toInt
    val N: Int = args(1).toInt
    val c: Array[Double] = new Array[Double](N)
    val b: Array[Double] = new Array[Double](M)
    val A: Array[Array[Double]] = Array.fill(M, N)(0.0)
    var j = 0
    while (j < N) {
        c(j) = StdRandom.uniform(1000)
          j += 1
    }
    var i = 0
    while (i < M) {
      b(i) = StdRandom.uniform(1000)
          i += 1
    }
    i = 0
    while (i < M) {
      j = 0
      while (j < N) {
            A(i)(j) = StdRandom.uniform(100)
              j += 1
        }
          i += 1
    }
    val lp = new Simplex[Double](A, b, c, EPSILON)
    println(lp.value)
  }

  def test1 {
    val A: Array[Array[Double]] = Array(Array(-1, 1, 0), Array(1, 4, 0), Array(2, 1, 0), Array(3, -4, 0), Array(0, 0, 1))
    val c: Array[Double] = Array(1, 1, 1)
    val b: Array[Double] = Array(5, 45, 27, 24, 4)
    test(A, b, c, EPSILON)
  }

  def test[T:Field:Order:ClassTag](A: Array[Array[T]], b: Array[T], c: Array[T], epsilon: T) {
    val lp = new Simplex[T](A, b, c, epsilon)
    println("value = " + lp.value)
    val x: Array[T] = lp.primal
    cforRange(0 until x.length) { i =>
      println("x[" + i + "] = " + x(i))
    }
    val y: Array[T] = lp.dual
    cforRange(0 until y.length) { j =>
      StdOut.println("y[" + j + "] = " + y(j))
    }
  }
    // x0 = 12, x1 = 28, opt = 800
  def test2 {
    val c: Array[Double] = Array(13.0, 23.0)
    val b: Array[Double] = Array(480.0, 160.0, 1190.0)
    val A: Array[Array[Double]] = Array(Array(5.0, 15.0), Array(4.0, 4.0), Array(35.0, 20.0))
    test(A, b, c, EPSILON)
  }

      // x0 = 12, x1 = 28, opt = 800
  def test2rat {
    val c: Array[Rational] = Array(13, 23)
    val b: Array[Rational] = Array(480, 160, 1190)
    val A: Array[Array[Rational]] = Array(Array(5, 15), Array(4, 4), Array(35, 20))
    test(A, b, c, Rational.zero)
  }

  // unbounded
  def test3 {
    val c: Array[Double] = Array(2.0, 3.0, -1.0, -12.0)
    val b: Array[Double] = Array(3.0, 2.0)
    val A: Array[Array[Double]] = Array(Array(-2.0, -9.0, 1.0, 9.0), Array(1.0, 1.0, -1.0, -2.0))
    test(A, b, c, EPSILON)
  }

  def test4 {
    val c: Array[Double] = Array(10.0, -57.0, -9.0, -24.0)
    val b: Array[Double] = Array(0.0, 0.0, 1.0)
    val A: Array[Array[Double]] = Array(Array(0.5, -5.5, -2.5, 9.0), Array(0.5, -1.5, -0.5, 1.0), Array(1.0, 0.0, 0.0, 0.0))
    test(A, b, c, EPSILON)
  }
}
