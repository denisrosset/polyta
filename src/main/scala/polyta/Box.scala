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

import qalg.algebra._
import qalg.algos._
import qalg.syntax.all._

case class Box[M, V, @sp(Double) A](lowerBounds: Bounds[V], upperBounds: Bounds[V])(implicit val MV: MatVecInField[M, V, A]) extends LinearConvexSet[M, V, A] {
  require(lowerBounds.v.length == upperBounds.v.length)
  def nX: Int = lowerBounds.v.length
}

object Box {
  def unbounded[M, V, @sp(Double) A](nX: Int)(implicit MV: MatVecInField[M, V, A]): Box[M, V, A] = {
    import MV.V
    Box[M, V, A](Bounds.unbounded[V, A](nX), Bounds.unbounded[V, A](nX))
  }
  def apply[M, V](lb: V, ub: V)(implicit MV: MatVecInField[M, V, Double]): Box[M, V, Double] = {
    import MV.V
    Box[M, V, Double](Bounds(lb), Bounds(ub))
  }
}
