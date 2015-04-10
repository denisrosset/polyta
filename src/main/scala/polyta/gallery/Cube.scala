package com.faacets
package polyta
package gallery

import scala.{specialized => sp}

import spire.algebra.Field
import spire.math.Rational
import spire.syntax.field._

import qalg.algebra._
import qalg.algos._

object Cube {
  def inV[M, V, @sp(Double, Long) A](dim: Int)(implicit M: MatVecInField[M, V, A]): VPolyhedron[M, V, A] = {
    import M.V
    implicit def A: Field[A] = M.scalar
    def gen(d: Int): List[List[Int]] = if (d == 0) List(Nil) else for {
      rest <- gen(d - 1)
      x <- List(0, 1)
    } yield x :: rest
    val vertices = M.vertcat(gen(dim).map(coeffs => M.build(1, dim, coeffs.map(A.fromInt(_)): _*)): _*)
    VPolyhedron.fromVertices(vertices)
  }
  def inH[M, V, @sp(Double, Long) A](dim: Int)(implicit M: MatVecInField[M, V, A]): HPolyhedron[M, V, A] = {
    import M.V
    implicit def A: Field[A] = M.scalar
    def oneAt(at: Int): V = V.fromFunV(new FunV[A] {
      def len = dim
      def f(k: Int): A = if (k == at) A.one else A.zero
    })
    def constraint1(k: Int) = LinearInequalityLE(oneAt(k), A.one)
    def constraint2(k: Int) = LinearInequalityLE(-oneAt(k), A.zero)
    HPolyhedron.fromLinearConstraints[M, V, A](dim, (0 until dim).flatMap(k => Seq(constraint1(k), constraint2(k))))
  }
}
