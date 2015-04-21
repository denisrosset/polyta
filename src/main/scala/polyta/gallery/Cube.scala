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
  def inV[M, V, @sp(Double, Long) A](dim: Int)(implicit M: MatVecInField[M, V, A]): VPolyhedronM[M, V, A] = {
    import M.V
    implicit def A: Field[A] = M.scalar
    def gen(d: Int): List[List[Int]] = if (d == 0) List(Nil) else for {
      rest <- gen(d - 1)
      x <- List(0, 1)
    } yield x :: rest
    val vertices = M.fromCols(dim, gen(dim).map(coeffs => V.build(coeffs.map(A.fromInt(_)): _*)): _*)
    VPolyhedronM.fromVertices(vertices)
  }
  def inH[M, V, @sp(Double, Long) A](dim: Int)(implicit M: MatVecInField[M, V, A]): HPolyhedronM[M, V, A] = {
    import M.V
    implicit def A: Field[A] = M.scalar
    def oneAt(at: Int): V = V.tabulate(dim)(k => if (k == at) A.one else A.zero)
    def constraint1(k: Int) = LinearInequalityLE(oneAt(k), A.one)
    def constraint2(k: Int) = LinearInequalityLE(-oneAt(k), A.zero)
    HPolyhedronM.fromLinearConstraints[M, V, A](dim, (0 until dim).flatMap(k => Seq(constraint1(k), constraint2(k))))
  }
}
