package com.faacets
package polyta

case class Box[A](lowerBounds: Bounds[A], upperBounds: Bounds[A]) extends ConvexSet[A] {
  require(lowerBounds.v.length == upperBounds.v.length)
  def dim: Int = lowerBounds.v.length
}

object Box {

  def unbounded[A](dim: Int)(implicit A: LinAlg[A]): Box[A] =
    Box[A](Bounds.unbounded[A](dim), Bounds.unbounded[A](dim))

}
