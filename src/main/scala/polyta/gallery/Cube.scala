package com.faacets
package polyta
package gallery

import scala.{specialized => sp}

import spire.algebra.{Field, Order}
import spire.math.Rational
import spire.syntax.field._

import qalg.algebra._
import qalg.algos._

object Cube {
  def inV[V, @sp(Double, Long) A: Order](dim: Int)(implicit pack: PackField.ForV[V, A]): VPolytope[V, A] = {
    import pack.A
    def gen(d: Int): List[List[A]] = if (d == 0) List(Nil) else for {
      rest <- gen(d - 1)
      x <- List(A.zero, A.one)
    } yield x :: rest
    val vertices = gen(dim).map(coeffs => VecBuild[V, A].build(coeffs: _*))
    VPolytope[V, A](dim, vertices, Seq.empty)
  }
  def inH[V, @sp(Double, Long) A](dim: Int)(implicit pack: PackField.ForV[V, A]): HPolytope[V, A] = {
    import pack.A
    def oneAt(at: Int): V = VecBuild[V, A].tabulate(dim)(k => if (k == at) A.one else A.zero)
    def constraint1(k: Int) = LinearInequality(oneAt(k), LE, A.one)
    def constraint2(k: Int) = LinearInequality(-oneAt(k), LE, A.zero)
    HPolytope[V, A](dim, (0 until dim).flatMap(k => Seq(constraint1(k), constraint2(k))), Seq.empty)
  }
}
