package com.faacets
package polyta
package gallery

import spire.algebra.Field

object Cube {

  def inV[A](dim: Int)(implicit A: LinAlg[A]): VPolytopeM[A] = {
    import A.{fieldA, IVec}
    def gen(d: Int): List[List[A]] = if (d == 0) List(Nil) else for {
      rest <- gen(d - 1)
      x <- List(Field[A].zero, Field[A].one)
    } yield x :: rest
    val vertices = gen(dim).map(coeffs => IVec.fromSeq(coeffs))
    VPolytopeM[A](dim, vertices, Seq.empty)
  }

  def inH[A](dim: Int)(implicit A: LinAlg[A]): HPolytopeM[A] = {
    import A.{fieldA, IVec}
    import ComparisonOp._
    def oneAt(at: Int): IVec = IVec.tabulate(dim)(k => if (k == at) Field[A].one else Field[A].zero)
    def constraint1(k: Int) = LinearInequality(oneAt(k), LE, Field[A].one)
    def constraint2(k: Int) = LinearInequality(-oneAt(k), LE, Field[A].zero)
    HPolytopeM[A](dim, (0 until dim).flatMap(k => Seq(constraint1(k), constraint2(k))), Seq.empty)
  }

}
