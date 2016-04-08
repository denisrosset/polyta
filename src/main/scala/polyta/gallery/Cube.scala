package com.faacets
package polyta
package gallery

import scala.{specialized => sp}

import spire.algebra.{Field, Order}
import spire.math.Rational
import spire.syntax.field._

import net.alasc.perms.Perm

import scalin.{Mat, Vec}

object Cube {

  def inV[A](dim: Int)(implicit A: LinAlg[A]): VPolytopeM[A] = {
    import A.{fieldA, orderA, IVec, IMat}
    def gen(d: Int): List[List[A]] = if (d == 0) List(Nil) else for {
      rest <- gen(d - 1)
      x <- List(fieldA.zero, fieldA.one)
    } yield x :: rest
    val vertices = gen(dim).map(coeffs => IVec.fromSeq(coeffs))
    VPolytopeM[A](dim, vertices, Seq.empty)
  }

  def inH[A](dim: Int)(implicit A: LinAlg[A]): HPolytopeM[A] = {
    import A.{fieldA, orderA, IVec, IMat}
    import ComparisonOp._
    def oneAt(at: Int): IVec = IVec.tabulate(dim)(k => if (k == at) fieldA.one else fieldA.zero)
    def constraint1(k: Int) = LinearInequality(oneAt(k), LE, fieldA.one)
    def constraint2(k: Int) = LinearInequality(-oneAt(k), LE, fieldA.zero)
    HPolytopeM[A](dim, (0 until dim).flatMap(k => Seq(constraint1(k), constraint2(k))), Seq.empty)
  }

}
