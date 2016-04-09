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

import scalin.{Mat, Vec}

case class Box[A](lowerBounds: Bounds[A], upperBounds: Bounds[A]) extends ConvexSet[A] {
  require(lowerBounds.v.length == upperBounds.v.length)
  def dim: Int = lowerBounds.v.length
}

object Box {

  def unbounded[A](dim: Int)(implicit A: LinAlg[A]): Box[A] =
    Box[A](Bounds.unbounded[A](dim), Bounds.unbounded[A](dim))

}
