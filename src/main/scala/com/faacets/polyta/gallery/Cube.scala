package com.faacets.polyta
package gallery

import spire.algebra.CRing

import scalin.immutable.{Mat, Vec}
import scalin.immutable.dense._

object Cube {

  def inV[A:CRing](dim: Int): VPolytope[A] = {
    def gen(d: Int): List[List[A]] = if (d == 0) List(Nil)
    else for {
      rest <- gen(d - 1)
      x <- List(CRing[A].zero, CRing[A].one)
    } yield x :: rest
    val vertices = gen(dim)
    val mV = Mat.tabulate[A](vertices.size, dim) { (r, c) => vertices(r)(c) }
    VPolytope.fromVertices(mV)
  }

  def inH[A:CRing](dim: Int): HPolytope[A] = {
    val mA = Mat.eye[A](dim).vertcat(-Mat.eye[A](dim))
    val vb = Vec.ones[A](dim).cat(Vec.zeros[A](dim))
    HPolytope.fromInequalities[A](mA, vb)
  }

}
