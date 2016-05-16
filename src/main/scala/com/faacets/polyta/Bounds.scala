package com.faacets.polyta

import scala.collection.BitSet

import scalin.Vec
import scalin.syntax.all._

case class Bounds[A](v: Vec[A], isBoundSet: BitSet)

object Bounds {

  def unbounded[A](dim: Int)(implicit A: LinAlg[A]): Bounds[A] = {
    import A.IVec
    Bounds(zeros[A](dim), BitSet.empty)
  }

}
