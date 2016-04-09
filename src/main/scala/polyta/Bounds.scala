package com.faacets
package polyta

import scala.{specialized => sp}

import scala.collection.BitSet
import scala.reflect.ClassTag

import spire.algebra.Eq
import spire.math.Rational
import spire.syntax.vectorSpace._
import spire.syntax.cfor._
import spire.util._

import scalin.Vec
import scalin.syntax.all._

case class Bounds[A](v: Vec[A], isBoundSet: BitSet)

object Bounds {

  def unbounded[A](dim: Int)(implicit A: LinAlg[A]): Bounds[A] = {
    import A.IVec
    Bounds(zeros[A](dim), BitSet.empty)
  }

}
