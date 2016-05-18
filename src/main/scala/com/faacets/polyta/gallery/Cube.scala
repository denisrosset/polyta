package com.faacets.polyta
package gallery

import spire.algebra.Field

import com.faacets.polyta.{ComparisonOp, LinAlg, LinearInequality}

import scalin.syntax.all._

object Cube {

  def inV[A](dim: Int)(implicit A: LinAlg[A]): VPolytope.Aux[A, Symmetry.Without.type] = {
    import A.{IMat, IVec, fieldA}
    def gen(d: Int): List[List[A]] = if (d == 0) List(Nil)
    else for {
      rest <- gen(d - 1)
      x <- List(Field[A].zero, Field[A].one)
    } yield x :: rest
    val vertices = gen(dim)
    val mV = tabulate[A](vertices.size, dim) { (r, c) => vertices(r)(c) }
    VPolytope(mV, zeros[A](0, dim))
  }

  def inH[A](dim: Int)(implicit A: LinAlg[A]): HPolytope.Aux[A, Symmetry.Without.type] = {
    import A.{IVec, IMat, fieldA}
    import ComparisonOp._
    def oneAt(at: Int): IVec = IVec.tabulate(dim)(k => if (k == at) Field[A].one else Field[A].zero)
    val mA = eye[A](dim) vertcat -eye[A](dim)
    val vb = ones[A](dim) cat zeros[A](dim)
    HPolytope[A](mA, vb, zeros[A](0, dim), zeros[A](0))
  }

}
