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
  def inV[M, V, @sp(Double, Long) A](dim: Int)(implicit alg: AlgMVF[M, V, A]): VPolyhedronM[M, V, A] = {
    implicit def A: Field[A] = alg.M.A
    def gen(d: Int): List[List[Int]] = if (d == 0) List(Nil) else for {
      rest <- gen(d - 1)
      x <- List(0, 1)
    } yield x :: rest
    val vertices = MatBuilder[M, A].fromCols(dim, gen(dim).map(coeffs => VecBuilder[V, A].build(coeffs.map(A.fromInt(_)): _*)): _*)
    VPolyhedronM.fromVertices(vertices)
  }
  def inH[M, V, @sp(Double, Long) A](dim: Int)(implicit alg: AlgMVF[M, V, A]): HPolyhedronM[M, V, A] = {
    implicit def A: Field[A] = alg.M.A
    def oneAt(at: Int): V = VecBuilder[V, A].tabulate(dim)(k => if (k == at) A.one else A.zero)
    def constraint1(k: Int) = LinearInequalityLE(oneAt(k), A.one)
    def constraint2(k: Int) = LinearInequalityLE(-oneAt(k), A.zero)
    HPolyhedronM.fromLinearConstraints[M, V, A](dim, (0 until dim).flatMap(k => Seq(constraint1(k), constraint2(k))))
  }
}
